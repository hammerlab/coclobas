open Internal_pervasives


module Cluster = Kube_cluster

module Job = Kube_job

module Error = struct
  let to_string =
    let exn = Printexc.to_string in
    function
    | `Shell (cmd, ex) ->
      sprintf "Shell-command failed: %S" cmd
    | `Storage e -> Storage.Error.to_string e
    | `IO (`Write_file_exn (path, e)) ->
      sprintf "Writing file %S: %s" path (exn e)
    | `IO (`Read_file_exn (path, e)) ->
      sprintf "Reading file %S: %s" path (exn e)
    | `Job e -> Job.Error.to_string e
    | `Start_server (`Exn e) ->
      sprintf "Starting Cohttp server: %s" (exn e)
end

module Server = struct

  type t = {
    port : int;
    mutable status: [ `Initializing | `Ready ] [@default `Initializing];
    root : string;
    mutable cluster : Cluster.t;
    mutable jobs: Job.t list;
    storage: Storage.t;
    log: Log.t;
    mutable job_list_mutex: Lwt_mutex.t option;
  } [@@deriving make]

  let log_event t e =
    let json_event name moar_json = 
      `Assoc ([
          "event", `String name;
          "date", `String (ODate.Unix.(now () |> Printer.to_iso));
        ] @ moar_json)
    in
    let stringf fmt = ksprintf (fun s -> `String s) fmt in
    let current_jobs () =
      `List (List.map t.jobs ~f:(fun j ->
          `Assoc [
            "id", `String (Job.id j);
            "status", (Job.status j |> Job.Status.to_yojson);
          ])) in
    let subsection, json =
      match e with
      | `Ready ->
        "startup",
        json_event "start-with-jobs" [
          "jobs", current_jobs ();
        ]
      | `Loop_begins todo ->
        "loop",
        json_event "loop-begins" [
          "todo", `List 
            (List.map todo ~f:(function
               | `Remove j -> stringf "rm %s" (Job.id j)
               | `Start j -> stringf "start %s" (Job.id j)
               | `Update j -> stringf "update %s" (Job.id j)));
          "jobs", current_jobs ();
        ]
      | `Loop_ends errors ->
        "loop",
        json_event "loop-ends" [
          "errors", `List (List.map errors ~f:(fun x ->
              `String (Error.to_string x)));
          "jobs", current_jobs ();
        ]
      | `Changed_job_list action ->
        let action =
          match action with
          | `Add j ->
            `Assoc ["verb", `String "add"; "job", `String (Job.id j)]
          | `Remove j ->
            `Assoc ["verb", `String "remove"; "job", `String (Job.id j)]
        in
        "job-list",
        json_event "changed-job-list" [
          "action", action;
          "jobs", current_jobs ();
        ]
    in
    Log.log t.log ~section:["server"; subsection] json

  let change_job_list t action =
    let mutex =
      Option.value_exn t.job_list_mutex ~msg:"job_list_mutex!! not initialized?" in
    Lwt_mutex.with_lock mutex begin fun () ->
      begin match action with
      | `Add j -> t.jobs <- j :: t.jobs
      | `Remove j ->
        t.jobs <- List.filter t.jobs ~f:(fun jj -> Job.id jj <> Job.id j);
      end;
      Storage.Json.save_jsonable t.storage
        ~path:["server"; "jobs.json"]
        (`List (List.map t.jobs ~f:(fun j -> `String (Job.id j))))
      >>= fun () ->
      log_event t (`Changed_job_list action)
    end

  let get_job_list t =
    let parse =
      let open Pvem.Result in
      function
      | `List l ->
        List.fold ~init:(return []) l ~f:(fun prev j ->
            prev >>= fun l ->
            match j with
            | `String s -> return (s :: l)
            | other -> fail "expecting List of Strings")
      | other -> fail "expecting List (of Strings)"
    in
    begin
      Storage.Json.get_json t.storage ~path:["server"; "jobs.json"] ~parse
      >>< function
      | `Ok ids -> return ids
      | `Error (`Storage (`Missing_data md)) -> return []
      | `Error other -> fail other
    end
    >>= fun ids ->
    Deferred_list.while_sequential ids ~f:(fun id ->
        Job.get t.storage id)
    >>= fun jobs ->
    t.jobs <- jobs;
    return ()

  let incoming_job t string =
    Storage.Json.parse_json_blob ~parse:Job.Specification.of_yojson string
    >>= fun spec ->
    let job = Job.make spec in
    Job.save (Storage.make t.root) job
    >>= fun () ->
    change_job_list t (`Add job)
    >>= fun () ->
    return `Done

  let rec loop:
    t -> (unit, [ `Storage of Storage.Error.common ]) Deferred_result.t
    = fun t ->
      let now () = Unix.gettimeofday () in
      let todo =
        List.fold t.jobs ~init:[] ~f:(fun prev j ->
            match Job.status j with
            | `Error _
            | `Finished _ -> `Remove j :: prev
            | `Submitted -> `Start j :: prev
            | `Started time when time +. 30. > now () -> prev
            | `Started _ -> `Update j :: prev
          )
      in
      log_event t (`Loop_begins (todo))
      >>= fun () ->
      Pvem_lwt_unix.Deferred_list.for_sequential todo ~f:begin function
      | `Remove j ->
        change_job_list t (`Remove j)
      | `Start j ->
        Job.start ~log:t.log j
        >>< begin function
        | `Ok () -> 
          j.Job.status <- `Started (now ());
          Job.save (Storage.make t.root) j
          >>= fun () ->
          return ()
        | `Error e ->
          j.Job.status <- `Error (Error.to_string e);
          Job.save (Storage.make t.root) j
          >>= fun () ->
          return ()
        end
      | `Update j ->
        begin
          Job.get_status_json ~log:t.log j
          >>= fun blob ->
          Job.Kube_status.of_json blob
          >>= fun stat ->
          let open Job.Kube_status in
          begin match stat with
          | { phase = `Pending }
          | { phase = `Unknown }
          | { phase = `Running } ->
            j.Job.status <- `Started (now ());
            Job.save (Storage.make t.root) j
            >>= fun () ->
            return ()
          | { phase = (`Failed | `Succeeded as phase)} ->
            j.Job.status <- `Finished (now (), phase);
            Job.save (Storage.make t.root) j
            >>= fun () ->
            return ()
          end
        end >>< begin function
        | `Ok () -> return ()
        | `Error e ->
          j.Job.status <- `Error (Error.to_string e);
          Job.save (Storage.make t.root) j
        end
      end
      >>= fun ((_ : unit list),
               (* We make sure only really fatal errors “exit the loop:” *)
               (errors : [ `Storage of Storage.Error.common ] list)) ->
      log_event t (`Loop_ends errors)
      >>= fun () ->
      begin match errors with
      | [] -> return ()
      | _ :: _ ->
        dbg "%s → ERRORS IN THE LOOP!!!: %s"
          ODate.Unix.(now () |> Printer.to_iso)
          (List.map errors ~f:Error.to_string |> String.concat ~sep:"\n");
        exit 5
      end
      >>= fun () ->
      (Pvem_lwt_unix.System.sleep 3. >>< fun _ -> return ())
      >>= fun () ->
      loop t

  let initialization t =
    begin match t.job_list_mutex with
    | Some s -> ()
    | None ->
      let m = Lwt_mutex.create () in
      t.job_list_mutex <- Some m;
    end;
    Cluster.ensure_living ~log:t.log t.cluster
    >>= fun () ->
    get_job_list t
    >>= fun () ->
    t.status <- `Ready;
    log_event t `Ready;
    >>= fun () ->
    Lwt.async (fun () -> loop t);
    return ()

  let get_job_status t ids =
    Deferred_list.while_sequential ids ~f:(fun id ->
        Job.get (Storage.make t.root) id
        >>= fun job ->
        return (`Assoc [
            "id", `String id;
            "status", Job.status job |> Job.Status.to_yojson;
          ]))
    >>= fun l ->
    return (`Json (`List l))

  let get_job_logs t ids =
    Deferred_list.while_sequential ids ~f:(fun id ->
        Job.get (Storage.make t.root) id
        >>= fun job ->
        Job.get_logs ~log:t.log job
        >>= fun (out, err) ->
        return (`Assoc [
            "id", `String id;
            "output", `String (out ^ err);
          ]))
    >>= fun l ->
    return (`Json (`List l))


  let get_job_description t ids =
    Deferred_list.while_sequential ids ~f:(fun id ->
        Job.get (Storage.make t.root) id
        >>= fun job ->
        Job.describe ~log:t.log job
        >>= fun descr ->
        return (`Assoc [
            "id", `String id;
            "description", `String descr;
          ]))
    >>= fun l ->
    return (`Json (`List l))


  let kill_jobs t ids =
    Deferred_list.while_sequential ids ~f:(fun id ->
        Job.get (Storage.make t.root) id
        >>= fun job ->
        Job.kill ~log:t.log job
        >>= fun () ->
        return ())
    >>= fun _ ->
    return `Done

  let respond_result r =
    let open Cohttp in
    let open Cohttp_lwt_unix in
    let open Lwt in
    r >>= begin function
    | `Ok `Done -> Server.respond_string ~status:`OK ~body:"Done" ()
    | `Ok (`Json j) ->
      let body = Yojson.Safe.pretty_to_string ~std:true j in
      Server.respond_string ~status:`OK ~body ()
    | `Error e ->
      Server.respond_string
        ~status:`Bad_request ~body:(Error.to_string e) ()
    end

  let job_ids_of_uri uri =
    Uri.query uri
    |> List.concat_map ~f:(function | ("id", l) -> l | _ -> [])

  let start t =
    let condition = Lwt_condition.create () in
    let server_thread () =
      Deferred_result.wrap_deferred
        ~on_exn:(fun e -> `Start_server (`Exn e))
        begin fun () ->
          let open Cohttp in
          let open Cohttp_lwt_unix in
          let open Lwt in
          let callback _conn req body =
            let uri = req |> Request.uri in
            match Uri.path uri with
            | "/status" ->
              let body =
                match t.status with
                | `Initializing -> "Initializing"
                | `Ready -> "Ready"
              in
              Server.respond_string ~status:`OK ~body ()
            | "/job/status" ->
              get_job_status t (job_ids_of_uri uri) |> respond_result
            | "/job/logs" ->
              get_job_logs t (job_ids_of_uri uri) |> respond_result
            | "/job/describe" ->
              get_job_description t (job_ids_of_uri uri) |> respond_result
            | "/job/kill" ->
              kill_jobs t  (job_ids_of_uri uri) |> respond_result
            | "/job/submit" ->
              body |> Cohttp_lwt_body.to_string
              >>= fun body_string ->
              incoming_job t body_string
              |> respond_result
            | other ->
              let meth = req |> Request.meth |> Code.string_of_method in
              let headers = req |> Request.headers |> Header.to_string in
              body |> Cohttp_lwt_body.to_string >|= (fun body ->
                  (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
                     (Uri.to_string uri) meth headers body))
              >>= (fun body -> Server.respond_string ~status:`OK ~body ())
          in
          Server.create ~mode:(`TCP (`Port t.port)) (Server.make ~callback ())
          >>= fun () ->
          Lwt_condition.signal condition (`Ok ());
          return ()
        end
    in
    Lwt.async server_thread;
    initialization t
    >>= fun () ->
    Lwt_condition.wait condition
end

let configure ~root ~cluster =
  let storage = Storage.make root in
  Cluster.save ~storage cluster

let cluster ~root action =
  let storage = Storage.make root in
  Cluster.get storage
  >>= fun cluster ->
  let log = Log.stored storage in
  begin match action with
  | `Start -> Cluster.gcloud_start ~log cluster
  | `Delete -> Cluster.gcloud_delete ~log cluster
  | `Describe -> Cluster.gcloud_describe ~log cluster
  end

let start_server ~root ~port =
  let storage = Storage.make root in
  Cluster.get storage
  >>= fun cluster ->
  let log = Log.stored storage in
  let server = Server.make ~storage ~log ~root ~cluster ~port () in
  Server.start server


let run_deferred d =
  match Lwt_main.run d with
  | `Ok () -> ()
  | `Error e ->
    eprintf "Error:\n   %s\n" (Error.to_string e);
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

(* let test_job_terms () = *)
(*   let open Cmdliner in *)
(*   let start = *)
(*     let term = *)
(*       let open Term in *)
(*       pure begin fun *)
(*         (`Root root) *)
(*         (`Image image) *)
(*         (`Output_job_id output_job_id) *)
(*         command -> *)
(*         run_deferred begin *)
(*           let job = Job.Specification.fresh ~image command in *)
(*           let storage = Storage.make root in *)
(*           Persist.save_job storage job *)
(*           >>= fun () -> *)
(*           printf ">>Job-ID: %s\n%!" (Job.id job); *)
(*           begin match output_job_id with *)
(*           | Some s -> Pvem_lwt_unix.IO.write_file s ~content:job.Job.id *)
(*           | None -> return () *)
(*           end *)
(*           >>= fun () -> *)
(*           Job.start job *)
(*         end *)
(*       end *)
(*       $ root_term () *)
(*       $ required_string "image" (fun s -> `Image s) *)
(*         ~doc:"The Docker image to use" *)
(*       $ begin *)
(*         pure (fun s -> `Output_job_id s) *)
(*         $ Arg.( *)
(*             value & opt (some string) None & *)
(*             info ["output-job-id"] ~doc:"Write the JOB-Id to a file") *)
(*       end *)
(*       $ Arg.( *)
(*           value & pos_all string [] *)
(*           & info [] ~doc:"The command to run" *)
(*         ) in *)
(*     let info = Term.(info "create-job" ~doc:"Register a job") in *)
(*     (term, info) in *)
(*   let job_cmd cmdname ~doc ~f = *)
(*     let term = *)
(*       let open Term in *)
(*       pure begin fun *)
(*         (`Root root) *)
(*         (`Id id) -> *)
(*         run_deferred begin *)
(*           let storage = Storage.make root in *)
(*           Persist.get_job storage id *)
(*           >>= fun job -> *)
(*           f job *)
(*         end *)
(*       end *)
(*       $ root_term () *)
(*       $ begin *)
(*         pure (fun s -> `Id s) *)
(*         $ Arg.(required & pos 0 (some string) None *)
(*                & info [] ~doc:"The command to run") *)
(*       end *)
(*     in *)
(*     let info = Term.(info cmdname ~doc) in *)
(*     (term, info) in *)
(*   let all_jobs_cmd cmdname ~doc ~f = *)
(*     let term = *)
(*       let open Term in *)
(*       pure begin fun (`Root root) -> *)
(*         run_deferred begin *)
(*           let storage = Storage.make root in *)
(*           Persist.all_job_ids storage *)
(*           >>= fun jobs -> *)
(*           List.fold jobs ~init:(return ()) ~f:(fun prevm id -> *)
(*               prevm >>= fun () -> *)
(*               Persist.get_job storage id *)
(*               >>= fun job -> *)
(*               f job) *)
(*         end *)
(*       end *)
(*       $ root_term () *)
(*     in *)
(*     let info = Term.(info cmdname ~doc) in *)
(*     (term, info) in *)
(*   let describe = *)
(*     job_cmd "describe-job" ~doc:"Get the description form Kube" ~f:(fun job -> *)
(*         Job.describe job *)
(*         >>= fun descr -> *)
(*         printf "### DESCRIPTION:\n%s\n%!" descr; *)
(*         return ()) in *)
(*   let status = *)
(*     job_cmd "status-job" ~doc:"Get the status form Kube" ~f:(fun job -> *)
(*         Job.get_status_json job *)
(*         >>= fun blob -> *)
(*         Job.Status.of_json blob *)
(*         >>= fun status -> *)
(*         printf "### STATUS:\n%s\n%!" *)
(*           (Job.Status.to_yojson status |> Yojson.Safe.pretty_to_string ~std:true); *)
(*         return ()) in *)
(*   let show_all = *)
(*     all_jobs_cmd "show-all" ~doc:"Show all jobs" ~f:(fun job -> *)
(*         begin *)
(*           Job.get_status_json job *)
(*           >>= fun blob -> *)
(*           Job.Status.of_json blob *)
(*         end >>< begin function *)
(*         | `Ok status -> *)
(*           printf "Job: %s (%s on %s): %s\n" *)
(*             job.Job.id *)
(*             (String.concat ~sep:" " job.Job.command) *)
(*             job.Job.image *)
(*             (Job.Status.show status); *)
(*           return () *)
(*         | `Error e -> *)
(*           printf "Job: %s (%s on %s): NO STATUS: %s\n" *)
(*             job.Job.id *)
(*             (String.concat ~sep:" " job.Job.command) *)
(*             job.Job.image *)
(*             (Error.to_string e); *)
(*             return () *)
(*       end) *)
(*   in *)
(*   [start; describe; status; show_all] *)


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
        run_deferred (start_server ~root ~port)
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
  let choices = [cluster; start_server; configure] (* @ test_job_terms () *) in
  match Term.eval_choice default_cmd choices with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
