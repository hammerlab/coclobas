open Internal_pervasives

type t = private {
  name : string;
  zone : string;
  min_nodes : int;
  max_nodes : int;
  machine_type : string;
}

val make :
  zone:string ->
  ?min_nodes:int -> max_nodes:int -> ?machine_type:string -> string -> t

val save :
  storage:Storage.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val get :
  Storage.t ->
  (t,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of
        [> `Exn of exn | `Missing_data of string | `Of_json of string ] ])
    Deferred_result.t

val gcloud_start :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val gcloud_delete :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val gcloud_describe :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val gcloud_set_current :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val ensure_living :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t
