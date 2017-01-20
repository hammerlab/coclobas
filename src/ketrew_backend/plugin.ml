
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

let local_docker_program ~base_url ~image ?(volume_mounts = []) p =
  let playground_path =
    let id = Uuidm.(v5 (create `V4) "coclojobs" |> to_string ~upper:false) in
    sprintf "/tmp/%s.sh" id in
  let extra_mount =
    `Local (playground_path, extra_mount_container_side) in
  create ~base_url ~program:p ~playground_path
    (Coclobas.Job.Specification.local_docker
       Coclobas.Local_docker_job.Specification.(
         make ~image
           ~volume_mounts:(extra_mount :: volume_mounts)
           ["sh"; sprintf "%s/%s" extra_mount_container_side script_filename]
       ))

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
                                   extra_mount_container_side)
                  |> to_string
                ) in
              System.ensure_directory_path playground
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
          Client.get_job_statuses client [job_id]
          >>= fun statuses ->
          begin match statuses with
          | [(_, st)] ->
            begin match st with
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

  let rec markup : t -> Ketrew_pure.Internal_pervasives.Display_markup.t =
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
          "Volumes",
          (List.map dock.volume_mounts ~f:(function
             | `Local (f, t) -> "Local", textf "%s:%s" f t)
           |> description_list);
        ]
    in
    function
    | `Created c ->
      description_list [
        "Client", uri c.client.Coclobas.Client.base_url;
        "Program", option ~f:Ketrew_pure.Program.markup c.program; 
        "Playground-path", option ~f:command c.playground_path; 
        "Job", job_spec c.specification;
      ]
    | `Running rp ->
      description_list [
        "Created-as", markup (`Created rp.created);
        "Job-ID", command rp.job_id;
      ]

  let log rp =
    ["Coclobas", Ketrew_pure.Internal_pervasives.Display_markup.log (markup rp)]


  let additional_queries :
    run_parameters -> (string * Ketrew_pure.Internal_pervasives.Log.t) list =
    fun rp ->
      let open Ketrew_pure.Internal_pervasives.Log in
      let common = [
        "ketrew-markup/display", s "Display the contents of the run-parameters";
        "server-status", s "Get the server status";
      ] in
      match rp with
      | `Created c -> common
      | `Running c ->
        (
          ("ketrew-markup/job-status", s "Get the “raw” job status")
          :: ("describe", s "Get a description of the job's current state")
          :: ("logs", s "Get the `logs` blob")
          :: common
        )


  let client_query m =
    m >>< function
    | `Ok o -> return o
    | `Error (`Client ce) ->
      fail (Ketrew_pure.Internal_pervasives.Log.verbatim (Client.Error.to_string ce))

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
      | "ketrew-markup/display", rp ->
        return (markup rp
                |> Ketrew_pure.Internal_pervasives.Display_markup.serialize)
      | "server-status", _ ->
        client_query begin
          Coclobas.Client.get_server_status_string created.client
        end
      | "ketrew-markup/job-status", `Running {job_id; _} ->
        client_query begin
          Client.get_job_statuses created.client
            [job_id]
          >>= fun l ->
          return Ketrew_pure.Internal_pervasives.Display_markup.(
              description_list
                (List.map l ~f:(fun (id, s) ->
                     "Job " ^ id,
                     (match s with
                     | `Error ee -> concat [text "Error: "; command ee]
                     | `Started d -> concat [text "Started on "; date d]
                     | `Finished (d, `Failed) ->
                       concat [text "Failed on "; date d]
                     | `Finished (d, `Succeeded) ->
                       concat [text "Succeeded on "; date d]
                     | `Finished (d, `Killed) ->
                       concat [text "Killed on "; date d]
                     | `Submitted -> text "Submitted")
                   ))
              |> serialize)
        end
      | "describe" , `Running {job_id; _} ->
        client_query begin
          Client.get_job_descriptions created.client [job_id]
          >>= fun l ->
          let rendered =
            List.map l ~f:(fun (`Id id, `Describe_output o, `Freshness frns) ->
                sprintf "### Job %s\n### Freshness: %s\n### Output:\n\n%s"
                  id frns o)
            |> String.concat ~sep:"\n"
          in
          return rendered
        end
      | "logs", `Running {job_id; _} ->
        client_query begin
          Client.get_job_logs created.client [job_id]
          >>= fun l ->
          let rendered =
            List.map l ~f:(fun (`Id id, `Describe_output o, `Freshness frns) ->
                sprintf "### Job %s\n### Freshness: %s\n### Output:\n\n%s"
                  id frns o)
            |> String.concat ~sep:"\n"
          in
          return rendered
        end
      | other, _ -> fail (s "Unknown query: " % s other)

end
let () =
  Ketrew.Plugin.register_long_running_plugin ~name (module Long_running_implementation)

include Long_running_implementation
