open Internal_pervasives
type t

val silent : t
val stored : Storage.t -> t

val log :
  ?section:string list ->
  t ->
  Yojson.Safe.json ->
  (unit,
   [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t
