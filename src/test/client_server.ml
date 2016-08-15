
open Nonstd
module String = Sosa.Native_string

let test_out fmt =
  ksprintf (fun s -> printf ">>>>> %s\n%!" s) fmt

let failf fmt =
  ksprintf (fun s -> Lwt.fail (Failure s)) fmt

let root = "_test/root"
let port = "8082"

let command ~bin args : Lwt_process.command =
  (bin, Array.of_list (bin :: args))

let coclobas args : Lwt_process.command =
  command ~bin:"./coclobas.byte" args

let curl ?post path =
  command ~bin:"curl" (
    [sprintf "http://localhost:%s/%s" port path]
    @ Option.value_map ~default:[] post ~f:(fun d ->
        ["--data-binary"; d])
  )

let rec curl_status_until_ready acc =
  let open Lwt in
  let curl_status =
    Lwt_process.open_process_in (curl "status")
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
    curl_status_until_ready ("Initializing" :: acc)
  | other ->
    failf "Curl /status: %s" (String.concat ~sep:"." curl_lines);
  end

module Coclojob = Coclobas.Command_line.Job.Specification

let curl_submit_job job =
  let open Lwt in
  let post = Coclojob.to_yojson job |> Yojson.Safe.pretty_to_string ~std:true in
  let process =
    Lwt_process.open_process_in
      ~stderr:`Dev_null
      (curl ~post "job/submit")
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_submit_job: %s %s"
    (Coclojob.show job) (String.concat ~sep:", " lines);
  return (job.Coclojob.id)

let curl_get_status ids =
  let open Lwt in
  let process =
    Lwt_process.open_process_in ~stderr:`Dev_null
      (ksprintf curl "job/status?%s"
         (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_statuses %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()

let curl_get_description ids =
  let open Lwt in
  let process =
    Lwt_process.open_process_in ~stderr:`Dev_null
      (ksprintf curl "job/describe?%s"
         (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_descr %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()


let curl_kill ids =
  let open Lwt in
  let process =
    Lwt_process.open_process_in ~stderr:`Dev_null
      (ksprintf curl "job/kill?%s"
         (List.map ids ~f:(sprintf "id=%s") |> String.concat ~sep:"&"))
  in
  Lwt_io.read_lines process#stdout |> Lwt_stream.to_list
  >>= fun lines ->
  test_out "curl_kill %s: %s"
    (String.concat ~sep:", " ids)
    (String.concat ~sep:"\n" lines);
  return ()

let config =
  coclobas [
    "config"; "--root"; root;
    "--cluster-name"; "sm-coclotest";
    "--cluster-zone"; "us-east1-c";
    "--max-nodes"; "5";
  ]

let job_with_nfs () =
  begin try
    let mount, witness =
      Sys.getenv "VALID_NFS_MOUNT"
      |> String.split ~on:(`Character ',')
      |> function
      | host :: path :: witness :: point :: [] ->
        Coclojob.Nfs_mount.make
          ~host ~path ~point (),
        sprintf "%s/%s" point witness
      | _ -> failwith "can't parse"
    in
    Some (
      Coclojob.fresh
        ~image:"ubuntu"
        ~volume_mounts:[`Nfs mount]
        ["ls"; "-la"; witness]
    )
  with _ -> None
  end


let () =
  Lwt_main.run Lwt.(
      let conf = Lwt_process.open_process_none config in
      conf#close
      >>= fun _ ->
      let server_process =
        Lwt_process.open_process_none
          (coclobas ["start-server"; "--root"; root; "--port"; port])
      in
      test_out "Server started";
      Lwt_unix.sleep 1.
      >>= fun () ->
      Lwt.pick [
        curl_status_until_ready [];
        Lwt_unix.sleep 12.
      ]
      >>= fun () ->
      curl_submit_job (Coclojob.fresh ~image:"ubuntu" ["sleep"; "42"])
      >>= fun sleep_42 ->
      curl_submit_job (Coclojob.fresh ~image:"ubuntu" ["du"; "-sh"; "/usr"])
      >>= fun du_sh_usr ->
      curl_get_status [sleep_42; du_sh_usr]
      >>= fun () ->
      Lwt_unix.sleep 5. >>= fun () ->
      curl_get_status [sleep_42; du_sh_usr]
      >>= fun () ->
      curl_kill [sleep_42]
      >>= fun () ->
      Lwt_unix.sleep 5. >>= fun () ->
      curl_get_description [sleep_42; du_sh_usr]
      >>= fun () ->
      Option.value_map ~default:(return ()) (job_with_nfs ())
        ~f:begin fun job ->
          curl_submit_job job
          >>= fun id ->
          curl_get_status [id]
          >>= fun () ->
          Lwt_unix.sleep 5.
          >>= fun () ->
          curl_get_status [id]
        end
      >>= fun () ->
      Lwt_unix.sleep 500.
      >>= fun () ->
      test_out "Killing server";
      server_process#kill Sys.sigint;
      server_process#close
      >>= fun _ ->
      return ()
    )
