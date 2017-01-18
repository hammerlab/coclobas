open Internal_pervasives

type t

val show : t -> string
val display_name : t -> string

val save :
  storage:Storage.t ->
  t ->
  (unit, [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val get :
  Storage.t ->
  (t, [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val kube : Kube_cluster.t -> t
val max_started_jobs : t -> int

val ensure_living :
  t ->
  log:Log.t ->
  (unit, [> `Log of Log.Error.t | `Shell_command of Hyper_shell.Error.t ])
    Deferred_result.t

val start :
  log:Log.t ->
  t ->
  (unit, [> `Log of Log.Error.t | `Shell_command of Hyper_shell.Error.t ])
    Deferred_result.t

val delete :
  log:Log.t ->
  t ->
  (unit, [> `Log of Log.Error.t | `Shell_command of Hyper_shell.Error.t ])
    Deferred_result.t

val describe :
  log:Log.t ->
  t ->
  (string * string,
   [> `Log of Log.Error.t | `Shell_command of Hyper_shell.Error.t ])
    Deferred_result.t

