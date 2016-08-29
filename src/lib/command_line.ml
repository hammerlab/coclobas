open Internal_pervasives

let (//) = Filename.concat
let db ~root =
  Storage.make (root // "db")
let log ~root =
  (* let st = Storage.make (root // "logs") in *)
  Log.file_tree (root // "logs")

let configure ~root ~cluster =
  let storage = db ~root in
  Kube_cluster.save ~storage cluster

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

let start_server ~root ~port =
  let storage = db root in
  Kube_cluster.get storage
  >>= fun cluster ->
  let log = log root in
  let server = Server.create ~storage ~log ~root ~cluster ~port in
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
  let choices = [cluster; start_server; configure] (* @ test_job_terms () *) in
  match Term.eval_choice default_cmd choices with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
