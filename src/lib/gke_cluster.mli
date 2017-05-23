open Internal_pervasives

type t = private {
  name : string;
  zone : string;
  min_nodes : int;
  max_nodes : int;
  machine_type : string;
  image_type: string option;
} [@@deriving yojson, show]

val make :
  zone:string ->
  ?min_nodes:int -> max_nodes:int ->
  ?machine_type: string ->
  ?image_type: string ->
  string -> t

val max_started_jobs: t -> int
(** The maximum number of jobs that Coclobas will attempt to run
    simultaneously on the cluster. *)


val gcloud_start :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val gcloud_delete :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val gcloud_describe :
  log:Log.t ->
  t ->
  (string * string,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val gcloud_set_current :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val ensure_living :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t
