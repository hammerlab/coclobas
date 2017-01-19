module Status : sig
  type t = [
    | `Submitted
    | `Started of float
    | `Finished of float * [ `Failed | `Succeeded | `Killed ]
    | `Error of string
  ] [@@deriving yojson,show ] 
end

module Specification : sig
  type t
  [@@deriving yojson,show ] 
  val kubernetes: Kube_job.Specification.t -> t
end

type t

val show : t -> string
 
val make :
  id:string ->
  ?status:Status.t ->
  ?update_errors:string list ->
  ?start_errors:string list -> Specification.t -> t

val id : t -> string
val status : t -> Status.t
val fresh : Specification.t -> t

val set_status : t -> Status.t -> unit

val start_errors : t -> string list
val set_start_errors : t -> string list -> unit

val update_errors : t -> string list
val set_update_errors : t -> string list -> unit

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
   | `Storage of [> Storage.Error.common ] ])
    Internal_pervasives.Deferred_result.t

val kill :
  log:Log.t ->
  t ->
  (unit, [> `Log of Log.Error.t | `Shell_command of Hyper_shell.Error.t ])
    Internal_pervasives.Deferred_result.t

val start :
  log:Log.t ->
  t ->
  (unit,
   [> `IO of [> `Write_file_exn of Pvem_lwt_unix.IO.path * exn ]
   | `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t ])
    Internal_pervasives.Deferred_result.t

val get_update :
  log:Log.t ->
  t ->
  ([> `Failed | `Running | `Succeeded ],
   [> `Job of
        [> `Kube_json_parsing of
             string * [> `Exn of exn | `String of string ] ]
   | `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t ])
    Internal_pervasives.Deferred_result.t
