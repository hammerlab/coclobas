
open Internal_pervasives

let to_string =
  let exn = Printexc.to_string in
  function
  | `Shell_command e ->
    sprintf "Shell-command failed:\n%s" (Hyper_shell.Error.to_display_string e)
  | `Storage e -> Storage.Error.to_string e
  | `Log e -> Log.Error.to_string e
  | `IO (`Write_file_exn (path, e)) ->
    sprintf "Writing file %S: %s" path (exn e)
  | `IO (`Read_file_exn (path, e)) ->
    sprintf "Reading file %S: %s" path (exn e)
  | `System _ as e ->
    Pvem_lwt_unix.System.error_to_string e
  | `Job e -> Kube_job.Error.to_string e
  | `Start_server (`Exn e) ->
    sprintf "Starting Cohttp server: %s" (exn e)
  | `Client err -> Client.Error.to_string err
  | `Invalid_job_submission (`Wrong_backend (`Kube, `Local_docker)) ->
    sprintf "Invalid job submission: backend mismatch: \
             job wants Kubernetes, cluster is Local-docker"
