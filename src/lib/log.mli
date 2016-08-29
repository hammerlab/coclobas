open Internal_pervasives
type t

val silent : t
val stored : Storage.t -> t

val file_tree: string -> t
(** Store the logs as simple files, under a given root-path. *)

module Error: sig

  type t =
    [ `IO of [ `Write_file_exn of string * exn ]
    | `System of
        [ `File_info of string
        | `List_directory of string
        | `Make_directory of string
        | `Remove of string ] *
        [ `Exn of exn | `Wrong_access_rights of int ]
    | `Storage of  Storage.Error.common 
    ]
    val to_string: [< t ] -> string

end

val log :
  ?section:string list ->
  t ->
  Yojson.Safe.json ->
  (unit, [> `Log of Error.t ]) Deferred_result.t

val empty: t ->
  (unit, [> `Log of Error.t ]) Deferred_result.t

val debug_sections : string list list ref
(** Catch sections and display them on ["stdout"], cf. the [?section] argument of {!log}.

    At the start of the program, the [Log] module tries to parse
    the ["COCLOBAS_DEBUG_SECTIONS"] environment variable to fill this
    reference (comma-separated list of paths, e.g. ["/server/loop,cluster//commands"]).
 *)
