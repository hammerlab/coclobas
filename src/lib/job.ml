open Internal_pervasives

module Specification = struct
  type t =
    | Kube of Kube_job.Specification.t
    | Local_docker of Local_docker_job.Specification.t
    | Aws_batch of Aws_batch_job.Specification.t
  [@@deriving yojson, show]
  let kind =
    function
    | Kube _ -> `Kube
    | Local_docker _ -> `Local_docker
    | Aws_batch _ -> `Aws_batch
  let kubernetes spec = Kube spec
  let local_docker spec = Local_docker spec

  let aws_batch spec = Aws_batch spec
end

module Status = struct
  type t = [
    | `Submitted
    | `Started of float
    | `Finished of float * [ `Failed | `Succeeded | `Killed ]
    | `Error of string
  ] [@@deriving yojson,show ] 
end

module State = struct
  type t =
    | Empty
    | Aws_batch_job of Aws_batch_job.State.t
  [@@deriving yojson, show]
end

type t = {
  id: string;
  specification: Specification.t [@main ];
  mutable status: Status.t [@default `Submitted];
  mutable update_errors : string list;
  mutable start_errors : string list;
  mutable latest_error: float option;
  mutable saved_state: State.t [@default State.Empty];
}
[@@deriving yojson, show, make]

let make ~id spec = make ~id spec

let id t = t.id
let status t = t.status
let set_status t ~from_error s =
  (if not from_error then t.latest_error <- None);
  t.status <- s

let start_errors t = t.start_errors
let set_start_errors t ~time l =
  t.latest_error <- Some time;
  t.start_errors <- l

let update_errors t = t.update_errors
let set_update_errors t ~time l =
  t.latest_error <- Some time;
  t.update_errors <- l

let latest_error t = t.latest_error

let fresh spec =
  let id = Uuidm.(v5 (create `V4) "coclojobs" |> to_string ~upper:false) in
  make ~id spec


let make_path id =
  (* TODO: use this in Kube_jobs uses of `~section` *)
  function
  | `Specification -> ["job"; id; "specification.json"]
  | `Status -> ["job"; id; "status.json"]
  | `Saved_state -> ["job"; id; "saved_state.json"]
  | `Describe_output -> ["job"; id; "describe.out"]
  | `Logs_output -> ["job"; id; "logs.out"]

let save st job =
  Storage.Json.save_jsonable st
    ~path:(make_path (id job) `Specification)
    (Specification.to_yojson job.specification)
  >>= fun () ->
  Storage.Json.save_jsonable st
    ~path:(make_path (id job) `Status)
    (Status.to_yojson job.status)
  >>= fun () ->
  Storage.Json.save_jsonable st
    ~path:(make_path (id job) `Saved_state)
    (State.to_yojson job.saved_state)

let get st job_id =
  Storage.Json.get_json st
    ~path:(make_path job_id `Specification)
    ~parse:Specification.of_yojson
  >>= fun specification ->
  Storage.Json.get_json st
    ~path:(make_path job_id `Status)
    ~parse:Status.of_yojson
  >>= fun status ->
  Storage.Json.get_json st
    ~path:(make_path job_id `Saved_state)
    ~parse:State.of_yojson
  >>= fun saved_state ->
  return {id = job_id; specification; status;
          update_errors = []; start_errors = [];
          latest_error = None; saved_state}

let kind t = Specification.kind t.specification

let aws_state t =
  match t.saved_state with
  | State.Empty -> fail (`Job (`Missing_aws_state t.id))
  | State.Aws_batch_job s -> return s

let get_logs ~storage ~log t =
  match kind t with
  | `Kube ->
    let save_path = make_path t.id `Logs_output in
    Kube_job.get_logs ~storage ~log ~id:t.id ~save_path
  | `Local_docker ->
    Local_docker_job.get_logs ~log ~id:t.id
  | `Aws_batch ->
    aws_state t
    >>= fun state ->
    Aws_batch_job.get_logs ~log ~id:t.id ~state

let describe ~storage ~log t =
  match kind t with
  | `Kube ->
    let save_path = make_path t.id `Describe_output in
    Kube_job.describe ~storage ~log ~id:t.id ~save_path
  | `Local_docker ->
    Local_docker_job.describe ~log ~id:t.id
  | `Aws_batch ->
    aws_state t
    >>= fun state ->
    Aws_batch_job.describe ~log ~id:t.id ~state

let kill ~log t =
  match kind t with
  | `Kube ->
    Kube_job.kill ~log ~id:t.id
  | `Local_docker ->
    Local_docker_job.kill ~log ~id:t.id
  | `Aws_batch ->
    aws_state t
    >>= fun state ->
    Aws_batch_job.kill ~log ~id:t.id ~state

let start ~log t ~cluster =
  match t.specification with
  | Specification.Kube specification ->
    Kube_job.start ~log ~id:t.id ~specification
  | Specification.Local_docker specification ->
    Local_docker_job.start ~log ~id:t.id ~specification
  | Specification.Aws_batch specification ->
    Aws_batch_job.start ~cluster ~log ~id:t.id ~specification
    >>= fun aws_state ->
    t.saved_state <- State.Aws_batch_job aws_state;
    return ()

let get_update ~log t =
  match kind t with
  | `Kube ->
    Kube_job.get_status_json ~log ~id:t.id
    >>= fun blob ->
    Kube_job.Kube_status.of_json blob
    >>= fun stat ->
    let open Kube_job.Kube_status in
    begin match stat with
    | { phase = `Pending }
    | { phase = `Unknown }
    | { phase = `Running } ->
      return `Running
    | { phase = (`Failed | `Succeeded as phase)} ->
      return phase
    end
  | `Local_docker ->
    Local_docker_job.get_update ~log ~id:t.id
  | `Aws_batch ->
    aws_state t
    >>= fun state ->
    Aws_batch_job.get_update ~log ~id:t.id ~state
