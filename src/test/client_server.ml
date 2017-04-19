
open Nonstd
module String = Sosa.Native_string

let test_out fmt =
  ksprintf (fun s -> printf ">>>>> %s\n%!" s) fmt

let failf fmt =
  ksprintf (fun s -> Lwt.fail (Failure s)) fmt

let command ~bin args : Lwt_process.command =
  (bin, Array.of_list (bin :: args))

let coclobas args : Lwt_process.command =
  command ~bin:"./coclobas.byte" args

let make_url port path =
  sprintf "http://localhost:%d/%s" port path

let curl ?post ~port path =
  command ~bin:"curl" (
    [make_url port path]
    @ Option.value_map ~default:[] post ~f:(fun d ->
        ["--data-binary"; d])
  )

let rec curl_status_until_ready ~port acc =
  let open Lwt in
  let curl_status =
    Lwt_process.open_process_in (curl ~port "status")
      ~stderr:`Dev_null
  in
  Lwt_io.read_lines curl_status#stdout |> Lwt_stream.to_list
  >>= fun curl_lines ->
  begin match curl_lines with
  | ["Ready"] ->
    test_out "curl_status_until_ready: %s → Ready"
      (String.concat ~sep:" → " acc);
    return ()
  | ["Initializing"] ->
    Lwt_unix.sleep 1.
    >>= fun () ->
    curl_status_until_ready ~port ("Initializing" :: acc)
  | other ->
    failf "Curl /status: %s" (String.concat ~sep:"." curl_lines);
  end

module Coclojob = Coclobas.Job.Specification

let submit_job ~port how job =
  let open Lwt in
  match how with
  | `Curl ->
    let post = Coclojob.to_yojson job |> Yojson.Safe.pretty_to_string ~std:true in
    let process =
      Lwt_process.open_process_in
        ~stderr:`Dev_null
        (curl ~port ~post "job/submit")
    in
    Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
    >>= fun lines ->
    test_out "curl_submit_job: %s %s"
      (Coclojob.show job) (String.concat ~sep:", " lines);
    return (List.hd_exn lines)
  | `Client ->
    Coclobas.Client.(submit_job (make (make_url port "")) job
      >>= fun res ->
      match res with
      | `Ok id -> return id
      | `Error (`Client c) -> failf "Client error: %s" (Error.to_string c)
    )

let curl_submit_job job = submit_job `Curl job

let get_status how ids ~port =
  let open Lwt in
  begin match how with
  | `Curl ->
    let process =
      Lwt_process.open_process_in ~stderr:`Dev_null
        (ksprintf (curl ~port) "job/status?%s"
           (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
    in
    Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  | `Client ->
    Coclobas.Client.(
      get_job_statuses
        (make (make_url port ""))
        ids
      >>= fun res ->
      match res with
      | `Ok stats ->
        return (List.map stats ~f:(fun (id, st) ->
            sprintf "%s: %s" id (Coclobas.Job.Status.show st)))
      | `Error (`Client c) -> failf "Client error: %s" (Error.to_string c)
    )
  end
  >>= fun lines ->
  test_out "get_statuses %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()

let curl_get_description ids ~port =
  let open Lwt in
  let process =
    Lwt_process.open_process_in ~stderr:`Dev_null
      (ksprintf (curl ~port) "job/describe?%s"
         (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_descr %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()

let curl_get_logs ids ~port =
  let open Lwt in
  let process =
    Lwt_process.open_process_in ~stderr:`Dev_null
      (ksprintf (curl ~port) "job/logs?%s"
         (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_descr %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()

let curl_kill ids ~port =
  let open Lwt in
  let process =
    Lwt_process.open_process_in ~stderr:`Dev_null
      (ksprintf (curl ~port) "job/kill?%s"
         (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_kill %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()


let make_test_job ~kind cmd =
  let image = "hammerlab/keredofi:epidisco-dev" in
  let open Coclobas in
  match kind with
  | `Aws_batch_queue ->
    Aws_batch_job.Specification.make ~image cmd |> Job.Specification.aws_batch
  | `GCloud_kubernetes ->
    Kube_job.Specification.make ~image cmd |> Job.Specification.kubernetes
  | `Local_docker ->
    Local_docker_job.Specification.make ~image cmd |> Job.Specification.local_docker

let job_with_nfs () =
  begin try
    let mount, witness =
      Sys.getenv "VALID_NFS_MOUNT"
      |> String.split ~on:(`Character ',')
      |> function
      | host :: path :: witness :: point :: [] ->
        Coclobas.Kube_job.Specification.Nfs_mount.make
          ~host ~path ~point (),
        sprintf "%s/%s" point witness
      | _ -> failwith "can't parse"
    in
    Some (
      Coclojob.kubernetes
        (Coclobas.Kube_job.Specification.make
           ~image:"ubuntu"
           ~volume_mounts:[`Nfs mount]
           ["ls"; "-la"; witness])
    )
  with _ -> None
  end

type test_config = {
  root: string;
  port: int [@default 22822];
} [@@deriving cmdliner]

let () =
  let main {root; port} = 
    Lwt_main.run Lwt.(
        let server_process =
          Lwt_process.open_process_none
            (coclobas ["start-server"; "--root"; root;
                       "--port"; Int.to_string port])
        in
        test_out "Server started";
        Lwt_unix.sleep 1.
        >>= fun () ->
        Lwt.pick [
          curl_status_until_ready ~port [];
          begin
            Lwt_unix.sleep 12.
            >>= fun () ->
            Lwt.fail_with "curl_status_until_ready timedout"
          end;
        ]
        >>= fun () ->
        begin
          Pvem_lwt_unix.Deferred_result.(
            Coclobas.Command_line.get_cluster ~root
            >>| Coclobas.Cluster.kind
          )
          >>= function
          | `Ok k -> return k
          | `Error e -> Coclobas.Error.to_string e |> Lwt.fail_with
        end
        >>= fun kind ->
        curl_submit_job ~port (make_test_job ~kind ["sleep"; "400"])
        >>= fun sleep_42 ->
        submit_job `Client ~port (make_test_job ~kind [
            "sh"; "-c"; "whoami ; \
                         du -sh /usr ; \
                         curl http://169.254.169.254/latest/meta-data/ \
                        "])
        >>= fun du_sh_usr ->
        get_status ~port `Curl [sleep_42; du_sh_usr]
        >>= fun () ->
        Lwt_unix.sleep 5. >>= fun () ->
        get_status ~port `Client [sleep_42; du_sh_usr]
        >>= fun () ->
        curl_kill ~port [sleep_42]
        >>= fun () ->
        Lwt_unix.sleep 5. >>= fun () ->
        curl_get_description ~port [sleep_42; du_sh_usr]
        >>= fun () ->
        Option.value_map ~default:(return ()) (job_with_nfs ())
          ~f:begin fun job ->
            curl_submit_job job ~port
            >>= fun id ->
            get_status `Curl [id] ~port
            >>= fun () ->
            Lwt_unix.sleep 5.
            >>= fun () ->
            get_status `Client [id] ~port
          end
        >>= fun () ->
        Lwt_unix.sleep 50.
        >>= fun () ->
        curl_get_description ~port [sleep_42; du_sh_usr]
        >>= fun () ->
        curl_get_logs ~port [sleep_42; du_sh_usr]
        >>= fun () ->
        test_out "Killing server";
        server_process#kill Sys.sigint;
        server_process#close
        >>= fun _ ->
        return ()
      )
  in
  match Cmdliner.Term.(eval (
      (pure main $ test_config_cmdliner_term ()),
      info "coclobas-client-server-test"
        ~doc:"A test that starts a server and talks to it."))
  with
  | `Error _ -> exit 1
  | `Ok ()
  | `Version
  | `Help -> exit 0

