open Internal_pervasives

let (//) = Filename.concat
let db ~root =
  Storage.make (root // "db")
let log ~root =
 Log.file_tree (root // "logs")

let configure ~root ~cluster ~server =
  let storage = db ~root in
  Kube_cluster.save ~storage cluster
  >>= fun () ->
  Server.Configuration.save ~storage server

let cluster ~root action =
  let storage = db ~root in
  Kube_cluster.get storage
  >>= fun cluster ->
  let log = log ~root in
  begin match action with
  | `Start ->
    Kube_cluster.gcloud_start ~log cluster
    >>= fun () ->
    printf "Cluster %s@%s: Started\n%!"
      cluster.Kube_cluster.name
      cluster.Kube_cluster.zone;
    return ()
  | `Delete ->
    Kube_cluster.gcloud_delete ~log cluster
    >>= fun () ->
    printf "Cluster %s@%s: Deleted\n%!"
      cluster.Kube_cluster.name
      cluster.Kube_cluster.zone;
    return ()
  | `Describe ->
    Kube_cluster.gcloud_describe ~log cluster
    >>= fun (out, err) ->
    printf "OUT:\n%s\nERR:\n%s\n%!" out err;
    return ()
  end


let client ~base_url action ids =
  let client = Client.{base_url} in
  begin match action with
  | `Describe ->
    Client.get_kube_job_descriptions client ids
    >>= fun descs ->
    List.iter descs
      ~f:(fun (`Id id, `Describe_output d, `Freshness f) ->
          printf "ID: %s\n\
                  Freshness: %s\n\
                  %s\n\n" id f d)
    |> return
  | `Status ->
    Client.get_kube_job_statuses client ids
    >>= fun statuses ->
    List.iter statuses ~f:(fun (r, s) ->
      printf "%s is %s\n" r (Kube_job.Status.show s))
    |> return
  | `List ->
    Client.get_job_list client
    >>= fun jobs ->
    List.iter jobs ~f:(fun (`Id i, `Status s)  ->
        printf "%s is %s\n" i s);
    return ()
  | `Kill ->
    Client.kill_kube_jobs client ids
  end


let start_server ~root ~port =
  let storage = db root in
  Kube_cluster.get storage
  >>= fun cluster ->
  Server.Configuration.get storage
  >>= fun configuration ->
  let log = log root in
  let server =
    Server.create ~configuration ~storage ~log ~root ~cluster ~port in
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

let client_term =
  let open Cmdliner in
  let term =
    let open Term in
    pure (fun (`Base_url base_url) action ids ->
        client ~base_url action ids
        |> run_deferred
      )
    $ required_string "server-url" (fun v -> `Base_url v)
      ~doc:"URL where the Cocolobas server can be found."
    $ Arg.(
        let actions = [
          "describe", `Describe;
          "status", `Status;
          "list", `List;
          "kill", `Kill;
        ] in
        required
        & pos 0 (some (enum actions)) None
        & info [] ~doc:"Action to do: {describe,status,list,kill}."
          ~docv:"ACTION"
      )
    $ Arg.(
        value & pos_right 0 string []
        & info [] ~doc:"Job IDs to act on."
          ~docv:"ID")
  in
  let info = Term.(info "client" ~doc:"Interact with the server.") in
  (term, info)


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
      ~doc:"Name of the Kubernetes cluster."
    $ required_string "cluster-zone" (fun s -> `Zone s)
      ~doc:"Zone of the Kubernetes cluster."
    $ begin
      pure (fun s -> `Max_nodes s)
      $ Arg.(
          required & opt (some int) None
          & info ["max-nodes"]
            ~doc:"Maximum number of nodes in the cluster." ~docv:"NUMBER")
    end
  in
  let server_config_term =
    let open Term in
    pure begin fun
      (`Max_update_errors max_update_errors)
      (`Min_sleep min_sleep)
      (`Max_sleep max_sleep) ->
      Server.Configuration.make ()
        ~min_sleep ~max_sleep ~max_update_errors
    end
    $ begin
      pure (fun s -> `Max_update_errors s)
        $ Arg.(value & opt int Server.Configuration.Default.max_update_errors &
               info ["max-update-errors"]
                 ~doc:"The number of `kubectl` errors allowed before \
                       considering  a job dead.")
    end
    $ begin
      pure (fun s -> `Min_sleep s)
        $ Arg.(value & opt float Server.Configuration.Default.min_sleep &
               info ["min-sleep"]
                 ~doc:"The minimal time to wait before reentering the \
                       “update loop” (events like job submission bypass this \
                       timer and wake-up the loop any way).")
    end
    $ begin
      pure (fun s -> `Max_sleep s)
        $ Arg.(value & opt float Server.Configuration.Default.max_sleep &
               info ["min-sleep"]
                 ~doc:"The maximal time to wait before reentering the \
                       “update loop.”")
    end
  in
  let configure =
    let term =
      let open Term in
      pure begin fun
        (`Root root)
        cluster
        server
        () ->
        configure ~root ~cluster ~server
        |> run_deferred
      end
      $ root_term ()
      $ cluster_term
      $ server_config_term
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
          & info [] ~doc:"Action to do on the current cluster:\
                         \ {start,describe,delete}."
            ~docv:"ACTION"
        )
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
  let choices = [cluster; start_server; configure; client_term] in
  match Term.eval_choice default_cmd choices with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
