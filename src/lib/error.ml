
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
  | `Job (`Kube_json_parsing _ as e) ->
    Kube_job.Error.to_string e
  | `Job (`Docker_inspect_json_parsing _ as e) ->
    Local_docker_job.Error.to_string e
  | `Job (`Missing_aws_state id) ->
    sprintf "Job %s is missing AWS-Batch data" id
  | `Start_server (`Exn e) ->
    sprintf "Starting Cohttp server: %s" (exn e)
  | `Client err -> Client.Error.to_string err
  | `Invalid_job_submission (`Wrong_backend (job, clu)) ->
    let disp =
      function
      | `Aws_batch_queue -> "AWS-Batch-Queue"
      | `Aws_batch -> "AWS-Batch-Job"
      | `Kube -> "Kubernetes"
      | `GCloud_kubernetes -> "GCloud-kubernetes"
      | `Local_docker -> "Local-Docker" in
    sprintf "Invalid job submission: backend mismatch: \
             job wants %s, cluster is %s"
      (disp job) (disp clu)
   | `Aws_batch_queue (`Check_valid, out, err) ->
     sprintf "Invalid AWS job-queue: %S (stderr: %S)" out err
   | `Aws_batch_job ((`Start _ | `Status _) as ae) ->
     sprintf "AWS-Batch-Job error: %s" (Aws_batch_job.Error.show ae)

