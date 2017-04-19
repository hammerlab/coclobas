open Internal_pervasives

type local_docker = private {max_jobs: int}
type t = private
  | Kube of Gke_cluster.t
  | Local_docker of local_docker
  | Aws_batch_queue of Aws_batch_queue.t
[@@deriving show]

val display_name : t -> string

val kind :
  t -> [> `Aws_batch_queue | `GCloud_kubernetes | `Local_docker ]

val save :
  storage:Storage.t ->
  t ->
  (unit, [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val get :
  Storage.t ->
  (t, [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val gke : Gke_cluster.t -> t

val local_docker: max_jobs:int -> t

val aws_batch_queue: Aws_batch_queue.t -> t

val max_started_jobs : t -> int

val ensure_living :
  t ->
  log:Log.t ->
  (unit,
   [> `Aws_batch_queue of [> `Check_valid ] * string * string
   | `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t ])
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

