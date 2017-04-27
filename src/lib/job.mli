module Status : sig
  type t = [
    | `Submitted
    | `Started of float
    | `Finished of float * [ `Failed | `Succeeded | `Killed ]
    | `Error of string
  ] [@@deriving yojson,show ] 
end

module Specification : sig
  type t = private
    | Kube of Kube_job.Specification.t
    | Local_docker of Local_docker_job.Specification.t
    | Aws_batch of Aws_batch_job.Specification.t
  [@@deriving yojson,show ] 
  val kind: t -> [ `Kube | `Local_docker | `Aws_batch ]
  val kubernetes: Kube_job.Specification.t -> t
  val local_docker: Local_docker_job.Specification.t -> t
  val aws_batch: Aws_batch_job.Specification.t -> t
end

type t
[@@deriving yojson,show ] 

val make : id: string -> Specification.t -> t
  

val id : t -> string
val status : t -> Status.t
val fresh : Specification.t -> t

val set_status : t -> from_error: bool -> Status.t -> unit

val start_errors : t -> string list
val set_start_errors : t -> time: float -> string list -> unit

val update_errors : t -> string list
val set_update_errors : t -> time: float -> string list -> unit

val latest_error: t -> float option

val save :
  Storage.t ->
  t ->
  (unit, [> `Storage of [> Storage.Error.common ] ])
    Internal_pervasives.Deferred_result.t
val get :
  Storage.t ->
  string ->
  (t, [> `Storage of [> Storage.Error.common ] ])
    Internal_pervasives.Deferred_result.t

val get_logs :
  storage:Storage.t ->
  log:Log.t ->
  t ->
  ([ `Archived of [ `Shell_command of Hyper_shell.Error.t ] | `Fresh ] *
   string,
   [> `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t
   | `Job of [> `Missing_aws_state of string ]
   | `Storage of [> Storage.Error.common ] ])
    Internal_pervasives.Deferred_result.t

val describe :
  storage:Storage.t ->
  log:Log.t ->
  t ->
  ([ `Archived of [ `Shell_command of Hyper_shell.Error.t ] | `Fresh ] *
   string,
   [> `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t
   | `Job of [> `Missing_aws_state of string ]
   | `Storage of [> Storage.Error.common ] ])
    Internal_pervasives.Deferred_result.t

val kill :
  log:Log.t ->
  t ->
  (unit, [> `Log of Log.Error.t
         | `Job of [> `Missing_aws_state of string ]
         | `Shell_command of Hyper_shell.Error.t ])
    Internal_pervasives.Deferred_result.t

val start :
  log:Log.t ->
  t ->
  cluster: Cluster.t ->
  (unit,
   [> `IO of [> `Write_file_exn of Pvem_lwt_unix.IO.path * exn ]
   | `Aws_batch_job of Aws_batch_job.Error.start
   | `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t ])
    Internal_pervasives.Deferred_result.t

val get_update :
  log:Log.t ->
  t ->
  ([> `Failed | `Running | `Succeeded ],
   [> `Job of
        [> `Docker_inspect_json_parsing of
             string * [> `Exn of exn | `String of string ]
        | `Kube_json_parsing of
             string * [> `Exn of exn | `String of string ] ]
   | `Log of Log.Error.t
   | `Aws_batch_job of Aws_batch_job.Error.status
   | `Job of [> `Missing_aws_state of string ]
   | `Shell_command of Hyper_shell.Error.t ])
    Internal_pervasives.Deferred_result.t
