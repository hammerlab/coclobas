open Internal_pervasives

type t

val make :
  port:int ->
  ?status:[ `Initializing | `Ready ] ->
  root:string ->
  cluster:Kube_cluster.t ->
  ?jobs:Kube_job.t list ->
  storage:Storage.t ->
  log:Log.t -> ?job_list_mutex:Lwt_mutex.t -> unit -> t

val start: t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int | `Exn of exn | `Signaled of int | `Stopped of int ]
   | `Storage of
        [> `Exn of exn
        | `Init_mkdir of
             [ `Exited of int
             | `Exn of exn
             | `Signaled of int
             | `Stopped of int ]
        | `Missing_data of string
        | `Of_json of string ] ]) Deferred_result.t
