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
   [> `Shell of
        string *
        [> `Exited of int | `Exn of exn | `Signaled of int | `Stopped of int ]
   | `Storage of [> Storage.Error.common
                 | `Missing_data of string
                 | `Of_json of string ] ]) Deferred_result.t
