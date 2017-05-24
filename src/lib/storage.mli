open Internal_pervasives

type t
type key = string list
type value = string

module Error : sig
  type where = [
    | `Update of key
    | `Read of key
    | `Parsing_json of string
  ]
  type common = [
    | `Exn of where * exn
    | `Backend of where * string
    | `Of_json of where * string
    | `Get_json of where * [ `Missing_data ]
  ]
  val to_string : [< common ] -> string
end

val make : string -> t

val update : t -> key -> value ->
  (unit,
   [> `Storage of [> Error.common] ]) Deferred_result.t

val read : t -> key ->
  (value option,
   [> `Storage of [> Error.common] ]) Deferred_result.t

val empty: t ->
  (unit,
   [> `Storage of[> Error.common] ]) Deferred_result.t

module Json : sig

  val save_jsonable : t -> path:key -> Yojson.Safe.json ->
    (unit,
     [> `Storage of [> Error.common ] ])
      Deferred_result.t

  val parse_json_blob :
    parse:(Yojson.Safe.json ->
           ('a, string) Ppx_deriving_yojson_runtime.Result.result) ->
    string ->
    ('a, [> `Storage of [> Error.common ] ]) Deferred_result.t

  val get_json_opt : t -> path:key ->
    parse:(Yojson.Safe.json ->
           ('a, string) Ppx_deriving_yojson_runtime.Result.result) ->
    ('a option,
     [> `Storage of [> Error.common]])
      Deferred_result.t
  val get_json : t -> path:key ->
    parse:(Yojson.Safe.json ->
           ('a, string) Ppx_deriving_yojson_runtime.Result.result) ->
    ('a,
     [> `Storage of [> Error.common]])
      Deferred_result.t
end

