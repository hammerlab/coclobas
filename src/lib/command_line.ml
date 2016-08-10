
open Nonstd
module String = Sosa.Native_string
open Pvem_lwt_unix.Deferred_result


module Generic_error = struct
  let to_string =
    function
    | `Exn e -> Printexc.to_string e
end

module Storage = struct
  module Store =
    Irmin_unix.Irmin_git.FS
      (Irmin.Contents.String)
      (Irmin.Ref.String)
      (Irmin.Hash.SHA1)

  type t = {
    root: string;
    mutable store: (string -> Store.t) option
  }

  let make root = {root; store = None}

  let wrap lwt = wrap_deferred lwt ~on_exn:(fun e -> `Storage (`Exn e))

  let init {root; _} =
    ksprintf Pvem_lwt_unix.System.Shell.do_or_fail "mkdir -p %s" root
    >>= fun () ->
    let config = Irmin_unix.Irmin_git.config ~root ~bare:true () in
    wrap (fun () -> Store.Repo.create config)
    >>= fun repo ->
    wrap (fun () -> Store.master Irmin_unix.task repo)

  let get_store t msg =
    match t.store with
    | Some f -> return (f msg)
    | None ->
      init t
      >>= fun store ->
      t.store <- Some store;
      return (store msg)

  let update t k v =
    let msg = sprintf "Update /%s" (String.concat ~sep:"/" k) in
    print_endline msg;
    get_store t msg
    >>= fun s ->
    wrap (fun () -> Store.update s k v)

  let read t k =
    let msg = sprintf "Read /%s" (String.concat ~sep:"/" k) in
    print_endline msg;
    get_store t msg
    >>= fun s ->
    wrap (fun () -> Store.read s k)

  let list t k =
    let msg = sprintf "List /%s" (String.concat ~sep:"/" k) in
    print_endline msg;
    get_store t msg
    >>= fun s ->
    wrap (fun () -> Store.list s k)
    

  module Error = struct
    include Generic_error
  end

end

module Cluster = struct
  type t = {
    name: string [@main];
    zone: string;
    min_nodes: int [@default 1];
    max_nodes: int;
  }
  [@@deriving yojson, show, make]

  let gcloud_start t =
    let cmd =
      sprintf 
        "gcloud container clusters create %s \
         --quiet --wait \
         --zone %s --num-nodes=%d --min-nodes=%d --max-nodes=%d \
         --enable-autoscaling"
        t.name t.zone t.min_nodes t.min_nodes t.max_nodes
    in
    Pvem_lwt_unix.System.Shell.do_or_fail cmd

  let gcloud_delete t =
    let cmd =
      sprintf 
        "gcloud container clusters delete --quiet --wait %s --zone %s" t.name t.zone in
    Pvem_lwt_unix.System.Shell.do_or_fail cmd

  let gcloud_describe t =
    let cmd =
      sprintf 
        "gcloud container clusters describe %s --zone %s" t.name t.zone in
    Pvem_lwt_unix.System.Shell.do_or_fail cmd


end

module Nfs_mount = struct
  type t = {
    host: string;
    path: string;
    point: string;
    read_only: bool [@default false];
  }
  [@@deriving yojson, show, make]

  let id m = Hashtbl.hash m |> sprintf "%s-%x" m.host
  let point m = m.point
  let host m = m.host
  let path m = m.path
  let point m = m.point
  let read_only m = m.read_only
end

module Job = struct
  type t = {
    id: string;
    image: string;
    command: string list;
    volume_mounts: [ `Nfs of Nfs_mount.t ] list;
  }
  [@@deriving yojson, show, make]

  let fresh ~image ?volume_mounts command =
    let id = Uuidm.(v5 (create `V4) "coclojobs" |> to_string ~upper:false) in
    make ~id ~image ?volume_mounts ~command ()

  let start t =
    let json : Yojson.Safe.json =
      `Assoc [
        "kind", `String "Pod";
        "apiVersion", `String "v1";
        "metadata", `Assoc [
          "name", `String t.id;
          "labels", `Assoc [
            "app", `String t.id;
          ];
        ];
        "spec", `Assoc [
          "restartPolicy", `String "Never";
          "containers", `List [
            `Assoc [
              "name", `String (t.id ^ "container");
              "image", `String t.image;
              "command", `List (List.map t.command ~f:(fun s -> `String s));
              "volumeMounts",
              `List (List.map t.volume_mounts ~f:(fun (`Nfs m) ->
                  `Assoc [
                    "name", `String (Nfs_mount.id m);
                    "mountPath", `String (Nfs_mount.point m);
                  ])
                );
            ]
          ];
          "volumes", `List (
            List.map t.volume_mounts ~f:(fun (`Nfs m) ->
                `Assoc [
                  "name", `String (Nfs_mount.id m);
                  "nfs", `Assoc [
                    "server", `String (Nfs_mount.host m);
                    "path", `String (Nfs_mount.path m);
                    "readOnly", `Bool (Nfs_mount.read_only m);
                  ];
                ])
          );
        ];
      ] in
    let json_string = Yojson.Safe.pretty_to_string ~std:true json in
    let tmp = Filename.temp_file "coclojob" ".json" in
    Pvem_lwt_unix.IO.write_file tmp ~content:json_string
    >>= fun () ->
    ksprintf Pvem_lwt_unix.System.Shell.do_or_fail "kubectl create -f %s" tmp

  let describe t =
    let tmp = Filename.temp_file "coclojob-descr" ".txt" in
    let cmd = sprintf "kubectl describe pod %s > %s" t.id tmp in
    Pvem_lwt_unix.System.Shell.do_or_fail cmd
    >>= fun () ->
    Pvem_lwt_unix.IO.read_file tmp

  let get_status_json t =
    let tmp = Filename.temp_file "coclojob-status" ".json" in
    ksprintf Pvem_lwt_unix.System.Shell.do_or_fail
      "kubectl get pod %s -o=json > %s" t.id tmp
    >>= fun () ->
    Pvem_lwt_unix.IO.read_file tmp

  module Status = struct
    (* cf. http://kubernetes.io/docs/user-guide/pod-states/ *)
    type t = {
      phase : [ `Pending | `Running | `Succeeded | `Failed | `Unknown ];
    }
    [@@deriving show,yojson]

    let phase_of_string =
      function
      | "Pending"   -> Some `Pending
      | "Running"   -> Some `Running
      | "Succeeded" -> Some `Succeeded
      | "Failed"    -> Some `Failed
      | "Unknown"   -> Some `Unknown
      | other -> None

    let of_json blob =    
      wrap_deferred
        ~on_exn:(fun e -> `Job (`Kube_json_parsing (blob, `Exn e)))
        (fun () -> Yojson.Safe.from_string blob |> Lwt.return)
      >>= fun json ->
      let fail_parsing reason =
        fail (`Job (`Kube_json_parsing (blob, `String reason))) in
      begin match json with
      | `Assoc l ->
        let phase =
          List.find_map l ~f:(function
            | "status", `Assoc json_assoc ->
              List.find_map json_assoc ~f:(function
                | "phase", `String phase -> Some phase
                | _ -> None)
            | _ -> None)
          |> Option.bind ~f:phase_of_string
        in
        begin match phase with
        | None -> fail_parsing "Cannot find field /status/phase"
        | Some phase -> return {phase}
        end
      | _ -> fail_parsing "JSON is not an `Assoc _"
      end
  end

  module Error = struct
    let to_string =
      function
      | `Kube_json_parsing (blob, `Exn e) ->
        sprintf "Parsing JSON output of kube-get-pod: %s, %s"
          (Printexc.to_string e)
          blob
      | `Kube_json_parsing (blob, `String e) ->
        sprintf "Parsing JSON output of kube-get-pod: %s, %s" e blob
  end

end


module Persist = struct


  let of_yojson_error = function
  | `Ok o -> return o
  | `Error s -> fail (`Persist (`Of_json s))
    
  let save_jsonable st ~path yo =
    let json = yo |> Yojson.Safe.pretty_to_string ~std:true in
    Storage.update st path json

  let get_json st ~path ~parse =
    Storage.read st path
    >>= begin function
    | Some json ->
      wrap_deferred ~on_exn:(fun e -> `Persist (`Exn e))
        (fun () -> Lwt.return (Yojson.Safe.from_string json))
      >>= fun yo ->
      of_yojson_error (parse yo)
    | None -> fail (`Persist (`Missing_data (String.concat ~sep:"/" path)))
    end
  
  let save_cluster st cluster =
    save_jsonable st (Cluster.to_yojson cluster) ~path:["cluster"]

  let get_cluster st =
    get_json st ~path:["cluster"] ~parse:Cluster.of_yojson

  let save_job st job =
    save_jsonable st (Job.to_yojson job) ~path:["job"; job.Job.id; "spec.json"]

  let get_job st id =
    get_json st ~path:["job"; id; "spec.json"] ~parse:Job.of_yojson

  let all_job_ids st =
    Storage.list st ["job"]
    >>= fun keys ->
    List.fold ~init:(return []) keys ~f:begin fun prevm k ->
      prevm >>= fun l ->
      match k with
      | "job" :: id :: [] -> return (id :: l)
      | other -> fail (`Persist (`Inconsistent_job_store (other, keys)))
    end


  module Error = struct
    let to_string =
      function
      | `Exn _ as gen -> Generic_error.to_string gen
      | `Of_json s -> sprintf "Persist.of_json: %s" s
      | `Missing_data s -> sprintf "Persist: missing data: %S" s
      | `Inconsistent_job_store (k, keys) ->
        sprintf "Inconsistent-job-store: found Key %S"
          (String.concat ~sep:"/" k)
  end
end



let configure ~root ~cluster =
  let storage = Storage.make root in
  Persist.save_cluster storage cluster

let cluster ~root action =
  let storage = Storage.make root in
  Persist.get_cluster storage
  >>= fun cluster ->
  begin match action with
  | `Start -> Cluster.gcloud_start cluster
  | `Delete -> Cluster.gcloud_delete cluster
  | `Describe -> Cluster.gcloud_describe cluster
  end

let server ~port =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let open Lwt in
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let run_deferred d =
  match Lwt_main.run d with
  | `Ok () -> ()
  | `Error e ->
    let exn = Printexc.to_string in
    eprintf "Error:\n   %s\n"
      begin match e with
      | `Shell (cmd, ex) ->
        sprintf "Shell-command failed: %S" cmd
      | `Persist e ->
        Persist.Error.to_string e
      | `Storage e -> Storage.Error.to_string e
      | `IO (`Write_file_exn (path, e)) ->
        sprintf "Writing file %S: %s" path (exn e)
      | `IO (`Read_file_exn (path, e)) ->
        sprintf "Reading file %S: %s" path (exn e)
      | `Job e -> Job.Error.to_string e
      end;
    exit 2

let required_string ~doc optname f =
  let open Cmdliner in
  let open Term in
  pure f
  $ Arg.(
      required & opt (some string) None
      & info [optname] ~doc)

let root_term () =
  required_string "root" (fun s -> `Root s)
    ~doc:"The root of the configuration"

let test_job_terms () =
  let open Cmdliner in
  let start =
    let term =
      let open Term in
      pure begin fun
        (`Root root)
        (`Image image)
        (`Output_job_id output_job_id)
        command ->
        run_deferred begin
          let job = Job.fresh ~image command in
          let storage = Storage.make root in
          Persist.save_job storage job
          >>= fun () ->
          printf ">>Job-ID: %s\n%!" job.Job.id;
          begin match output_job_id with
          | Some s -> Pvem_lwt_unix.IO.write_file s ~content:job.Job.id
          | None -> return ()
          end
          >>= fun () ->
          Job.start job
        end
      end
      $ root_term ()
      $ required_string "image" (fun s -> `Image s)
        ~doc:"The Docker image to use"
      $ begin
        pure (fun s -> `Output_job_id s)
        $ Arg.(
            value & opt (some string) None &
            info ["output-job-id"] ~doc:"Write the JOB-Id to a file")
      end
      $ Arg.(
          value & pos_all string []
          & info [] ~doc:"The command to run"
        ) in
    let info = Term.(info "create-job" ~doc:"Register a job") in
    (term, info) in
  let job_cmd cmdname ~doc ~f =
    let term =
      let open Term in
      pure begin fun
        (`Root root)
        (`Id id) ->
        run_deferred begin
          let storage = Storage.make root in
          Persist.get_job storage id
          >>= fun job ->
          f job
        end
      end
      $ root_term ()
      $ begin
        pure (fun s -> `Id s)
        $ Arg.(required & pos 0 (some string) None
               & info [] ~doc:"The command to run")
      end
    in
    let info = Term.(info cmdname ~doc) in
    (term, info) in
  let all_jobs_cmd cmdname ~doc ~f =
    let term =
      let open Term in
      pure begin fun (`Root root) ->
        run_deferred begin
          let storage = Storage.make root in
          Persist.all_job_ids storage
          >>= fun jobs ->
          List.fold jobs ~init:(return ()) ~f:(fun prevm id ->
              prevm >>= fun () ->
              Persist.get_job storage id
              >>= fun job ->
              f job)
        end
      end
      $ root_term ()
    in
    let info = Term.(info cmdname ~doc) in
    (term, info) in
  let describe =
    job_cmd "describe-job" ~doc:"Get the description form Kube" ~f:(fun job ->
        Job.describe job
        >>= fun descr ->
        printf "### DESCRIPTION:\n%s\n%!" descr;
        return ()) in
  let status =
    job_cmd "status-job" ~doc:"Get the status form Kube" ~f:(fun job ->
        Job.get_status_json job
        >>= fun blob ->
        Job.Status.of_json blob
        >>= fun status ->
        printf "### STATUS:\n%s\n%!"
          (Job.Status.to_yojson status |> Yojson.Safe.pretty_to_string ~std:true);
        return ()) in
  let show_all =
    all_jobs_cmd "show-all" ~doc:"Show all jobs" ~f:(fun job ->
        Job.get_status_json job
        >>= fun blob ->
        Job.Status.of_json blob
        >>= fun status ->
        printf "Job: %s (%s on %s): %s\n"
          job.Job.id
          (String.concat ~sep:" " job.Job.command)
          job.Job.image
          (Job.Status.show status);
        return ())
  in
  [start; describe; status; show_all]


let main () =
  let open Cmdliner in
  let version = "0.0.0" in
  let cluster_term =
    let open Term in
    pure begin fun
      (`Name name)
      (`Zone zone)
      (`Max_nodes max_nodes) ->
      Cluster.make name ~zone ~max_nodes
    end
    $ required_string "cluster-name" (fun s -> `Name s)
      ~doc:"Name of the Kubernetes cluster"
    $ required_string "cluster-zone" (fun s -> `Zone s)
      ~doc:"Zone of the Kubernetes cluster"
    $ begin
      pure (fun s -> `Max_nodes s)
      $ Arg.(
          required & opt (some int) None
          & info ["max-nodes"]
            ~doc:"Maximum number of nodes in th cluster" ~docv:"NUMBER")
    end
  in
  let configure =
    let term =
      let open Term in
      pure begin fun
        (`Root root)
        cluster
        () ->
        configure ~root ~cluster
        |> run_deferred
      end
      $ root_term ()
      $ cluster_term
      $ pure () in
    let info = Term.(info "configure" ~doc:"Configure an instance") in
    (term, info) in
  let cluster =
    let term =
      let open Term in
      pure begin fun
        (`Root root)
        action
        ->
          cluster ~root action
          |> run_deferred
      end
      $ root_term ()
      $ Arg.(
          let actions = [
            "start", `Start;
            "describe", `Describe;
            "delete", `Delete;
          ] in
          required
          & pos 0 (some (enum actions)) None
          & info [] ~doc:"Action to do on the current cluster")
    in
    let info = Term.(info "cluster" ~doc:"Manage the configured cluster") in
    (term, info) in
  let start_server =
    let term =
      let open Term in
      pure begin fun
        (`Root root)
        (`Port port)
        () ->
        Lwt_main.run (server ~port)
      end
      $ root_term ()
      $ begin
        pure (fun s -> `Port s)
        $ Arg.(
            required & opt (some int) None
            & info ["port"; "p"]
              ~doc:"The port to start the HTTP server" ~docv:"NUMBER")
      end
      $ pure ()
    in
    let info = Term.(info "start-server" ~doc:"Start the server") in
    (term, info) in
  let default_cmd =
    let doc = "Coclobas, a batch scheduler" in
    let man = [
      `S "AUTHORS";
      `P "Sebastien Mondet <seb@mondet.org>"; `Noblank;
      `S "BUGS";
      `P "Browse and report new issues at"; `Noblank;
      `P "<https://github.com/hammerlab/coclobas>.";
    ] in
    Term.(ret (pure (`Help (`Plain, None)))),
    Term.info Sys.argv.(0) ~version ~doc ~man in
  let choices = [cluster; start_server; configure] @ test_job_terms () in
  match Term.eval_choice default_cmd choices with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
