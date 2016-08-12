open Internal_pervasives
type t

val silent : t
val stored : Storage.t -> t

val log :
  ?section:string list ->
  t ->
  Yojson.Safe.json ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t
