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


val debug_sections : string list list ref
(** Catch sections and display them on ["stdout"], cf. the [?section] argument of {!log}.

    At the start of the program, the [Log] module tries to parse
    the ["COCLOBAS_DEBUG_SECTIONS"] environment variable to fill this
    reference (coma-separated list of paths, e.g. ["/server/loop,cluster//commands"]).
 *)
