open Internal_pervasives


let configure ~root ~cluster =
  let storage = Storage.make root in
  Kube_cluster.save ~storage cluster

let cluster ~root action =
  let storage = Storage.make root in
  Kube_cluster.get storage
  >>= fun cluster ->
  let log = Log.stored storage in
  begin match action with
  | `Start -> Kube_cluster.gcloud_start ~log cluster
  | `Delete -> Kube_cluster.gcloud_delete ~log cluster
  | `Describe -> Kube_cluster.gcloud_describe ~log cluster
  end

let start_server ~root ~port =
  let storage = Storage.make root in
  Kube_cluster.get storage
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
      Kube_cluster.make name ~zone ~max_nodes
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
