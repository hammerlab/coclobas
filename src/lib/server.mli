open Internal_pervasives

type t

val create :
  port:int ->
  root:string ->
  cluster:Kube_cluster.t ->
  storage:Storage.t ->
  log:Log.t ->
  t

val start: t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Storage of [> Storage.Error.common
                 | `Missing_data of string
                 | `Of_json of string ] ]) Deferred_result.t
