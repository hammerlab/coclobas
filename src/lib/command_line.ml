
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
end
module Job = struct
  type t = {
    id: string;
    image: string;
    command: string list;
    volume_mounts: [ `Nfs of Nfs_mount.t ]
  }
  [@@deriving yojson, show, make]
end


module Persist = struct


  let of_yojson_error = function
  | `Ok o -> return o
  | `Error s -> fail (`Persist (`Of_json s))
    
  
  let save_cluster st cluster =
    let json =
      Cluster.to_yojson cluster |> Yojson.Safe.pretty_to_string ~std:true in
    Storage.update st ["cluster"] json

  let get_cluster st =
    Storage.read st ["cluster"]
    >>= begin function
    | Some json ->
      wrap_deferred ~on_exn:(fun e -> `Persist (`Exn e))
        (fun () -> Lwt.return (Yojson.Safe.from_string json))
      >>= fun yo ->
      of_yojson_error (Cluster.of_yojson yo)
    | None -> fail (`Persist (`Missing_data "cluster"))
    end

  module Error = struct
    let to_string =
      function
      | `Exn _ as gen -> Generic_error.to_string gen
      | `Of_json s -> sprintf "Persist.of_json: %s" s
      | `Missing_data s -> sprintf "Persist: missing data: %S" s
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
    eprintf "Error:\n   %s\n"
      begin match e with
      | `Shell (cmd, ex) ->
        sprintf "Shell-command failed: %S" cmd
      | `Persist e ->
        Persist.Error.to_string e
      | `Storage e -> Storage.Error.to_string e
      end;
    exit 2

let main () =
  let open Cmdliner in
  let version = "0.0.0" in
  let required_string ~doc optname f =
    let open Term in
    pure f
    $ Arg.(
        required & opt (some string) None
        & info [optname] ~doc)
  in
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
  let root_term =
    required_string "root" (fun s -> `Root s)
      ~doc:"The root of the configuration" in
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
      $ root_term
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
      $ root_term
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
      $ root_term
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
  let choices = [cluster; start_server; configure] in
  match Term.eval_choice default_cmd choices with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
