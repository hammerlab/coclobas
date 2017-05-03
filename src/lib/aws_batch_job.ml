
open Internal_pervasives

module Specification = struct
  (* Cf.
     http://docs.aws.amazon.com/batch/latest/APIReference/API_ContainerProperties.html
  *)
  module File_contents_mount = Kube_job.Specification.File_contents_mount
  type t = {
    image: string;
    memory: [ `MB of int ] [@default `MB 128];
    cpus: int [@default 7];
    job_role: [ `Arn of string ] option;
    priviledged: bool [@default false];
    command: string list [@main];
    (* Mounting volumes seems to be only for host directories. *)
    volume_mounts: [ `S3_constant of File_contents_mount.t ] list;
  } [@@deriving yojson, show, make]
end

module State = struct
  type t = {
    aws_id: string;
  } [@@deriving yojson, show, make]
end

module Error = struct
  type nonrec exn = exn [@@deriving show]
  type start = [
      `Start of string * Specification.t *
                [ `Json_parsing of
                    string * [ `Exn of exn | `String of string ]
                | `Invalid_cluster of string * Cluster.t ]
  ] [@@deriving show]
  let start ~id ~specification e =
    `Aws_batch_job (`Start (id, specification, e))
  type status = [
      `Status of [`Id of string] * [ `Aws_id of string ]
                 * [ `Parsing_status of string ]
  ] [@@deriving show]
  let status ~id ~aws_id e =
    `Aws_batch_job (`Status (`Id id, `Aws_id aws_id, e))
  type t = [ start | status ] [@@deriving show]
end

let command_must_succeed ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed ~log cmd ?additional_json
    ~section:["job"; id; "commands"]
let command_must_succeed_with_output ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd ?additional_json
    ~section:["job"; id; "commands"]

let job_definition id = sprintf "coclodef-%s" id
let job_name id = sprintf "coclojob-%s" id

let canonicalize p =
  String.split ~on:(`Character '/') p
  |> begin function
  | [] | [""] | [""; ""] -> [""; ""]
  | "" :: more -> "" :: List.filter more ~f:((<>) "")
  | other -> List.filter other ~f:((<>) "")
  end
  |> String.concat ~sep:"/"

let start ~cluster ~log ~id ~specification =
  let open Specification in
  Cluster.( match cluster with
    | Kube _
    | Local_docker _ ->
      fail (Error.start ~id ~specification (`Invalid_cluster
                                              ("Not an AWS-queue", cluster)))
    | Aws_batch_queue q ->
      Aws_batch_queue.(queue_name q, s3_bucket q) |> return)
  >>= fun (job_queue, s3_bucket_opt) ->
  Deferred_list.while_sequential specification.volume_mounts ~f:begin function
  | `S3_constant {File_contents_mount. path; contents} ->
    begin match s3_bucket_opt with
    | None ->
      fail (Error.start ~id ~specification
              (`Invalid_cluster ("No S3 bucket configured", cluster)))
    | Some s -> return s
    end
    >>= fun s3_prefix ->
    let s3_filename =
      sprintf "Coclo-%s-%s-%d-%s"
        id Digest.(path ^ contents ^ id |> string |> to_hex)
        Random.(int 100_000_000)
        Filename.(basename path) in
    let local_path = sprintf "/tmp/%s" s3_filename in
    IO.write_file local_path ~content:contents
    >>= fun () ->
    let s3_uri = Uri.of_string s3_prefix in
    let bucket = Uri.host_with_default ~default:"" s3_uri in
    let s3_prefix_path = Uri.path s3_uri |> canonicalize in
    let s3_full_path = Filename.concat s3_prefix_path s3_filename in
    ksprintf
      (command_must_succeed ~log ~id)
      "aws s3 cp %s s3://%s%s \
       --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers"
      local_path bucket s3_full_path
    >>= fun () ->
    let download_cmd =
      let mkdir =
        sprintf " mkdir -m 777 -p %s || echo mkdir-failed "
          (Filename.dirname path) in
      let curl =
        let url_path =
          Filename.concat s3_prefix_path s3_filename in
        sprintf "curl http://%s.s3-website-us-east-1.amazonaws.com%s -o %s"
          bucket url_path path in
      sprintf "{ echo 'Downloading %s' ; { %s ; sudo %s ; %s || sudo %s ;} \
               || echo 'Downloading %s failed' ; } "
        s3_full_path
        mkdir mkdir curl curl
        s3_full_path
    in
    return (download_cmd)
  end
  >>= fun (s3_commands : string list) ->
  let json =
    let string s : Yojson.Safe.json = `String s in
    let int s : Yojson.Safe.json = `Int s in
    let command =
      match s3_commands with
      | [] -> specification.command
      | more ->
        [
          "bash"; "-c";
          String.concat ~sep:" && "
            (s3_commands @ [List.map ~f:Filename.quote specification.command
                            |> String.concat ~sep:" "])
        ]
    in
    let props =
      [
        "image", string specification.image;
        "vcpus", int specification.cpus;
        "memory", int (specification.memory |> fun (`MB i) -> i);
        "command", `List (List.map ~f:string command);
        "privileged", `Bool specification.priviledged;
      ]
      @ Option.value_map ~default:[] specification.job_role ~f:(function
        | `Arn s -> ["jobRoleArn", string s])
    in
    `Assoc props in
  ksprintf
    (command_must_succeed ~log ~id)
    "aws batch register-job-definition --job-definition %s \
     --type container \
     --container-properties %s
    "
    (job_definition id)
    (Filename.quote (Yojson.Safe.to_string ~std:true json))
  >>= fun () ->
  ksprintf
    (command_must_succeed_with_output ~log ~id)
    "aws batch submit-job --job-name %s --job-queue %s --job-definition %s"
    (job_name id) job_queue (job_definition id)
  >>= fun (out, err) ->
  Deferred_result.wrap_deferred
    ~on_exn:(fun e ->
        Error.start ~id ~specification (`Json_parsing (out, `Exn e)))
    (fun () -> Yojson.Safe.from_string out |> Lwt.return)
  >>= fun json ->
  let job_id_opt =
    match json with
    | `Assoc l ->
      List.find_map l ~f:(function
        | ("jobId", `String v) -> Some v
        | other -> None)
    | other -> None
  in
  begin match job_id_opt with
  | None ->
    fail (Error.start ~id ~specification
            (`Json_parsing (out, `String "Can't find jobId")))
  | Some v -> return v
  end
  >>= fun aws_id ->
  return { State.aws_id }

let get_update ~log ~id ~state =
  ksprintf
    (command_must_succeed_with_output ~log ~id)
    "aws batch describe-jobs --jobs %s  --query 'jobs[0].status'"
    state.State.aws_id
  >>= fun (out, err) ->
  let status =
    String.filter out ~f:(function '"' | '\n' | ' ' -> false | _ -> true) in
  begin match status with
  | "SUBMITTED"
  | "PENDING"
  | "RUNNABLE"
  | "STARTING"
  | "RUNNING" -> return `Running
  | "SUCCEEDED" -> return `Succeeded
  | "FAILED" -> return `Failed
  | _ ->
    fail (Error.status ~id ~aws_id:state.State.aws_id (`Parsing_status status))
  end

let describe ~log ~id ~state =
  ksprintf
    (command_must_succeed_with_output ~log ~id)
    "aws batch describe-jobs --jobs %s"
    state.State.aws_id
  >>= fun (out, err) ->
  return (`Fresh, out)

let get_logs ~log ~id ~state =
  let cloudwatch_url =
    sprintf
      "https://console.aws.amazon.com/cloudwatch/home?\
       region=us-east-1#logStream:group=/aws/batch/job;prefix=%s/%s/;\
       streamFilter=typeLogStreamPrefix"
      (job_name id) state.State.aws_id
  in
  return (`Fresh, cloudwatch_url)

let kill ~log ~id ~state =
  ksprintf
    (command_must_succeed ~log ~id)
    "aws batch terminate-job --job-id %s --reason %s"
    state.State.aws_id
    (ksprintf Filename.quote "Killed by Coclobas on %s"
       ODate.Unix.(now () |> Printer.to_iso))