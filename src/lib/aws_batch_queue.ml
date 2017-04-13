open Internal_pervasives

type t = {
  max_jobs: int;
  queue_name: string;
} [@@deriving make, yojson, show]

let max_started_jobs t = t.max_jobs

let queue_name t = t.queue_name

let command_must_succeed ~log cluster cmd =
  Hyper_shell.command_must_succeed ~log cmd
    ~section:["cluster"; "commands"]
    ~additional_json:[
      "cluster", to_yojson cluster
    ]

let command_must_succeed_with_output ~log cluster cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd
    ~section:["cluster"; "commands"]
    ~additional_json:[
      "cluster", to_yojson cluster
    ]

let ensure_living t ~log =
  let cmd =
    sprintf "aws batch describe-job-queues \
             --job-queues %s \
             --query jobQueues[0].status --output text"
      t.queue_name in
  command_must_succeed_with_output ~log t cmd
  >>= fun (out, err) ->
  begin match out with
  | "VALID\n" -> return ()
  | other -> fail (`Aws_batch_queue (`Check_valid, other, err))
  end

