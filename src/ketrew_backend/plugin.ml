
open Coclobas
open Internal_pervasives
let (//) = Filename.concat

let name = "coclobas"

module Run_parameters = struct

  type created = {
    client: Client.t;
    specification: Job.Specification.t [@main];
    program: Ketrew_pure.Program.t option;
    playground_path: string option;
  } [@@deriving yojson, make]
  type running = {
    created: created;
    job_id: string;
  } [@@deriving yojson]
  type t = [
    | `Created of created
    | `Running of running
  ] [@@deriving yojson]

  let show t = to_yojson t |> Yojson.Safe.pretty_to_string

  let serialize run_parameters =
    to_yojson run_parameters
    |> Yojson.Safe.pretty_to_string ~std:true

  let deserialize_exn s =
    let open Ppx_deriving_yojson_runtime.Result in
    Yojson.Safe.from_string s |> of_yojson
    |> function
    | Ok o -> o
    | Error e -> failwith e
end

let create ~base_url ?playground_path ?program specification =
  let created =
    Run_parameters.make_created ?playground_path
      ?program ~client:(Client.make base_url) specification in
  `Long_running (name, `Created created |> Run_parameters.serialize)

let kubernetes_program ~base_url ~image ?(volume_mounts = []) p =
  let script_path = "/coclo-kube/mount/script" in
  let script =
    Kube_job.Specification.File_contents_mount.make
      ~path:script_path
      Ketrew_pure.Monitored_script.(
        create p
          ~playground:(Ketrew_pure.Path.absolute_directory_exn
                         "/tmp/playground")
        |> to_string
      ) in
  let spec =
    Kube_job.Specification.make
      ~image
      ~volume_mounts:(`Constant script :: volume_mounts)
      ["bash"; script_path]
    |> Job.Specification.kubernetes
  in
  create ~base_url ~program:p spec

let extra_mount_container_side = "/coclobas-ketrew-plugin-playground"
let script_filename = "program-monitored-script"

let local_docker_program
    ?(shell = "bash")
    ?cpus ?memory ?tmp_dir ~base_url ~image ?(volume_mounts = []) p =
  let tmp_dir =
    match tmp_dir with
    | Some d -> d
    | None -> try Sys.getenv "TMPDIR" with _ -> "/tmp/coclolocal"
  in
  let playground_dir =
    let id = Uuidm.(v5 (create `V4) "coclojob" |> to_string ~upper:false) in
    sprintf "%s-playground" id in
  let extra_mount =
    `Local (tmp_dir, extra_mount_container_side) in
  let playground_path = tmp_dir // playground_dir in
  create ~base_url ~program:p ~playground_path
    (Coclobas.Job.Specification.local_docker
       Coclobas.Local_docker_job.Specification.(
         make ~image
           ~volume_mounts:(extra_mount :: volume_mounts)
           ?cpus ?memory
           [shell;
            extra_mount_container_side // playground_dir // script_filename]
       ))

let aws_batch_program ~base_url ~image ?(volume_mounts = [])
    ?(shell = "bash")
    ?memory ?cpus ?job_role p =
  let script_path = "/tmp/coclobas-script.sh" in
  let cmd = [shell; script_path ] in
  let script =
    Ketrew_pure.Monitored_script.(
      create p
        ~playground:(Ketrew_pure.Path.absolute_directory_exn "/tmp")
      |> to_string
    ) in
  create ~base_url ~program:p (* ~playground_path *)
    (Coclobas.Job.Specification.aws_batch
       Coclobas.Aws_batch_job.Specification.(
         let extra_mount =
           `S3_constant (File_contents_mount.make ~path:script_path (script)) in
         make ~priviledged:true ?memory ?cpus ?job_role
           ~image ~volume_mounts:(extra_mount :: volume_mounts) cmd))

module Long_running_implementation : Ketrew.Long_running.LONG_RUNNING = struct

  type run_parameters = Run_parameters.t
  include Run_parameters

  let name = "coclobas"

  module KLRU = Ketrew.Long_running_utilities

  let classify_client_error m =
    m >>< function
    | `Ok o -> return o
    | `Error (`Client (`IO_exn _ as cl)) -> fail (`Recoverable (Client.Error.to_string cl))
    | `Error (`Client (`Response _ as cl)) -> fail (`Recoverable (Client.Error.to_string cl))
    | `Error (`Client cl) -> fail (`Fatal (Client.Error.to_string cl))
    | `Error (`Coclo_plugin (`Expecting_one_status l)) ->
      fail (`Fatal (sprintf "Expecting one status but got: [%s]"
                      (List.map l ~f:(fun (id, st) -> id)
                       |> String.concat ~sep:"; ")))
    | `Error (`Coclo_plugin (`Preparing_script msg)) ->
      fail (`Fatal (sprintf "Preparing script: %s" msg))

  let start :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    (run_parameters, Ketrew.Host_io.Error.classified) Deferred_result.t =
    fun rp ~host_io ->
      match rp with
      | `Running _ ->
        ksprintf KLRU.fail_fatal "start on already running: %s"
          (Run_parameters.show rp)
      | `Created ({client; specification;
                   program; playground_path} as created) ->
        classify_client_error begin
          begin match Job.Specification.kind specification,
                      playground_path, program with
          | `Local_docker, None, Some _ ->
            fail (`Coclo_plugin (`Preparing_script
                                   "Program provided but missing playground!"))
          | `Local_docker, Some playground, Some prog ->
            begin
              let script =
                Ketrew_pure.Monitored_script.(
                  create prog
                    ~playground:(Ketrew_pure.Path.absolute_directory_exn
                                   playground)
                  |> to_string
                ) in
              (* Permissions set to 0o777 because very often, inside
                 the docker container, we have a different user. *)
              System.ensure_directory_path ~perm:0o777 playground
              >>= fun () ->
              IO.write_file ~content:script (playground // script_filename)
            end >>< begin function
            | `Ok () -> return ()
            | `Error e ->
              let msg =
                sprintf "I/O/System Error: %s"
                  (match e with
                  | `IO _ as e -> IO.error_to_string e
                  | `System _ as e -> System.error_to_string e) in
              fail (`Coclo_plugin (`Preparing_script msg))
            end
          | `Aws_batch, _, _ ->
            return ()
          | `Local_docker, _, None (* No program means, no need for playground *)
          | `Kube, _, _ -> return ()
          end
          >>= fun () ->
          Client.submit_job client specification
          >>= fun job_id ->
          return (`Running {created; job_id})
        end

  let running rp f =
    match rp with
    | `Created _ ->
      ksprintf KLRU.fail_fatal "update on not started: %s"
        (Run_parameters.show rp)
    | `Running {created = {client; specification; _}; job_id; _} ->
      f job_id client specification

  let update :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    ([ `Failed of run_parameters * string
     | `Still_running of run_parameters
     | `Succeeded of run_parameters ], Ketrew.Host_io.Error.classified)
      Deferred_result.t =
    fun rp ~host_io ->
      running rp begin fun job_id client spec ->
        classify_client_error begin
          Client.get_job_states client [job_id]
          >>= fun jobs ->
          begin match jobs with
          | [(_, job)] ->
            begin match Job.status job with
            | `Finished (_,`Succeeded) ->
              return (`Succeeded rp)
            | `Error e ->
              return (`Failed (rp, sprintf "Error-from-coclobas: %s" e))
            | `Finished (_, (`Failed | `Killed)) ->
              return (`Failed (rp, "job-failed-or-killed"))
            | `Submitted
            | `Started _ -> return (`Still_running rp)
            end
          | other ->
            fail (`Coclo_plugin (`Expecting_one_status other))
          end
        end
      end

  let kill :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    ([ `Killed of run_parameters ], Ketrew.Host_io.Error.classified)
      Deferred_result.t =
    fun rp ~host_io ->
      running rp begin fun id client spec ->
        classify_client_error begin
          Client.kill_jobs client [id]
          >>= fun () ->
          return (`Killed rp)
        end
      end

  let rec markup ?status t =
    let open Ketrew_pure.Internal_pervasives.Display_markup in
    let job_spec js =
      let open Coclobas.Job.Specification in
      let open Coclobas.Kube_job.Specification in
      let open Coclobas.Local_docker_job.Specification in
      let nfs_mount nfs =
        let open Nfs_mount in
        description_list  [
          "Host", uri nfs.host;
          "Path", command nfs.path;
          "Point", command nfs.point;
          "Read-only", textf "%b" nfs.read_only;
          "Id", command (id nfs);
        ] in
      let constant_mount cst =
        let open File_contents_mount in
        description_list  [
          "Path", uri cst.path;
          "Contents", code_block cst.contents;
        ] in
      match js with
      | Kube kube ->
        description_list [
          "Image", uri kube.image;
          "Command", command (String.concat ~sep:" " kube.command);
          "Memory", (match kube.memory with `GB x -> textf "%d GB" x);
          "CPUs", textf "%d" kube.cpus;
          "Volumes",
          (List.map kube.volume_mounts ~f:(function
             | `Nfs nfs -> "NFS", nfs_mount nfs
             | `Constant cst -> "Constant", constant_mount cst)
           |> description_list);
        ]
      | Local_docker dock ->
        description_list [
          "Image", uri dock.image;
          "Command", command (String.concat ~sep:" " dock.command);
          "CPUS", option dock.cpus ~f:(textf "%.3f");
          "Memory", option dock.memory (
            function
            | `GB x -> textf "%d GiB" x
            | `MB x -> textf "%d MiB" x
          );
          "Volumes",
          (List.map dock.volume_mounts ~f:(function
             | `Local (f, t) -> "Local", textf "%s:%s" f t)
           |> description_list);
        ]
      | Aws_batch aws ->
        let open Coclobas.Aws_batch_job.Specification in
        description_list [
          "Image", uri aws.image;
          "Command", command (String.concat ~sep:" " aws.command);
          "Memory", (match aws.memory with `MB x -> textf "%d MB" x);
          "CPUs", textf "%d" aws.cpus;
          "Privileged", textf "%b" aws.priviledged;
          "Job-role", option aws.job_role ~f:(function
            | `Arn s -> text s);
          "Volumes",
          (List.map aws.volume_mounts ~f:(function
             | `S3_constant c -> "S3-Constant", constant_mount c)
           |> description_list);
        ]
    in
    match t with
    | `Created c ->
      let status_mu =
        Option.value status ~default:(text "Created (not sent to Coclobas)") in
      [
        "Status", status_mu;
        "Client", uri c.client.Coclobas.Client.base_url;
        "Program", option ~f:Ketrew_pure.Program.markup c.program; 
        "Playground-path", option ~f:command c.playground_path; 
        "Job", job_spec c.specification;
      ]
    | `Running rp ->
      ["Job-ID", command rp.job_id;]
      @ markup ?status (`Created rp.created)


  let log rp =
    [
      "Coclobas",
      Ketrew_pure.Internal_pervasives.Display_markup.(
        description_list (markup rp) |> log)
    ]

  module Query_names = struct
    let details_status = "ketrew-markup/Job details and status"
    let server_status = "ketrew-markup/Server status"
    let describe = "ketrew-markup/Call `describe`"
    let logs = "ketrew-markup/Call `logs`"
  end

  let additional_queries :
    run_parameters -> (string * Ketrew_pure.Internal_pervasives.Log.t) list =
    fun rp ->
      let open Ketrew_pure.Internal_pervasives.Log in
      let open Query_names in
      let common = [
        details_status, s "Display the contents and the status of the job";
        server_status, s "Get the server status";
      ] in
      match rp with
      | `Created c -> common
      | `Running c ->
        List.concat [
          common;
          [
            describe, s "Get a description of the job's current state";
            logs, s "Get the `logs` blob";
          ];
        ]


  let client_query m =
    m >>< function
    | `Ok o -> return o
    | `Error (`Client ce) ->
      fail (Ketrew_pure.Internal_pervasives.Log.verbatim (Client.Error.to_string ce))

  let job_query_result_to_markup l =
    let open Ketrew_pure.Internal_pervasives.Display_markup in
    List.map l ~f:(fun (id, qr) ->
        description_list
          (("Job-id", command id)
           :: List.map qr ~f:(fun (section, result) ->
               section,
               begin match result with
               | `Url u -> uri u
               | `Saved_command s ->
                 let open Hyper_shell.Saved_command in
                 let code_block_or_empty =
                   function
                   | s when String.strip s = "" -> text "Empty"
                   | other -> code_block other in
                 let should_display_archived = ref true in
                 let command_and_result = [
                   "Command", command s.command;
                   begin match s.outcome with
                   | `Error e ->
                     "Error",
                     let open Hyper_shell.Error in
                     description_list [
                       "STDOUT", option ~f:code_block_or_empty e.stdout;
                       "STDERR", option ~f:code_block_or_empty e.stderr;
                       "Status", 
                       option e.status
                         ~f:(text_of_stringable Pvem_lwt_unix.System.Shell.status_to_string);
                       "Exception", option e.exn ~f:command;
                     ]
                   | `Ok (out, err) ->
                     begin
                       if Some (out ^ err) =
                          Option.map ~f:Output_archive.to_string s.archived
                       then should_display_archived := false
                     end;
                     "Success", description_list [
                       "STDOUT", code_block_or_empty out;
                       "STDERR", code_block_or_empty err;
                     ]
                   end;
                 ] in
                 let archived =
                   let display_archive a =
                     let open Output_archive in
                     description_list [
                       "On", date a.date;
                       "STDOUT", code_block_or_empty a.out;
                       "STDERR", code_block_or_empty a.err;
                     ] in
                   if !should_display_archived then [
                     "Archived content", option ~f:display_archive s.archived;
                   ] else [] in
                 command_and_result @ archived |> description_list
               end)))
    |> concat
    |> serialize



  module Markup = Ketrew_pure.Internal_pervasives.Display_markup

  let markup_job_state : Coclobas.Job.t -> Markup.t = fun job ->
    let open Markup in
    let status =
      match Coclobas.Job.status job with
      | `Error ee -> concat [text "Error: "; command ee]
      | `Started d -> concat [text "Started on "; date d]
      | `Finished (d, `Failed) ->
        concat [text "Failed on "; date d]
      | `Finished (d, `Succeeded) ->
        concat [text "Succeeded on "; date d]
      | `Finished (d, `Killed) ->
        concat [text "Killed on "; date d]
      | `Submitted -> text "Submitted"
    in
    let error_list = function
    | [] -> text "None."
    | more ->
      let length_encoding =
        List.fold more ~init:[] ~f:(fun prev cur ->
            match prev with
            | (hn, hv) :: t when hv = cur -> (hn + 1, hv) :: t
            | _ -> (1, cur) :: prev)
        |> List.rev in
      itemize (List.map length_encoding ~f:(fun (x, err) ->
          concat [textf "%d × " x; code_block err]))
    in
    description_list [
      "Engine-status", status;
      "Start-errors", Coclobas.Job.start_errors job |> error_list;
      "Update-errors", Coclobas.Job.update_errors job |> error_list;
    ]

  let query :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    string ->
    (string, Ketrew_pure.Internal_pervasives.Log.t) Deferred_result.t =
    fun rp ~host_io query ->
      let open Ketrew_pure.Internal_pervasives.Log in
      let created =
        match rp with `Created c -> c | `Running {created; _} -> created in
      match query, rp with
      | ds, `Created _ when ds = Query_names.details_status ->
        return Markup.(markup rp |> description_list |> serialize)
      | ds, `Running {job_id; _} when ds = Query_names.details_status ->
        client_query begin
          Client.get_job_states created.client [job_id]
          >>= fun l ->
          let open Markup in
          let status =
            match l with
            | [_, one] -> markup_job_state one
            | other ->
              description_list [
                "ERROR-WRONG-NUMBER-OF-STATUSES", 
                concat ~sep:(text ", ")
                  (List.map l ~f:(fun (id, s) ->
                       concat [textf "Job %s: " id; markup_job_state s]));
              ]
          in
          return Markup.(markup rp ~status |> description_list |> serialize)
        end
      | ds, _ when ds = Query_names.server_status ->
        client_query begin
          Coclobas.Client.get_server_status_string created.client
        end
        >>= fun engine_status ->
        client_query (Coclobas.Client.get_job_list created.client)
        >>= fun job_list ->
        client_query (Coclobas.Client.get_cluster_description created.client)
        >>= fun cluster_description ->
        let status =
          let open Markup in
          description_list [
            "Engine-status", command engine_status;
            "Cluster", code_block cluster_description;
            "Jobs",
            begin match job_list with
            | [] -> text "0 currently active"
            | jobs ->
              itemize
                (List.map jobs ~f:(fun (`Id is, `Status s) ->
                     ksprintf command "%s: %s" is s))
            end;
          ] in
        return (status |> Markup.serialize)
      | ds, `Running {job_id; _} when ds = Query_names.describe ->
        client_query begin
          Client.get_job_descriptions created.client [job_id]
          >>| job_query_result_to_markup
        end
      | ds, `Running {job_id; _} when ds = Query_names.logs ->
        client_query begin
          Client.get_job_logs created.client [job_id]
          >>| job_query_result_to_markup
        end
      | other, _ -> fail (s "Unknown query: " % s other)

end
let () =
  Ketrew.Plugin.register_long_running_plugin ~name
    (module Long_running_implementation)

include Long_running_implementation
