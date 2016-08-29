open Internal_pervasives

type t
type key = string list
type value = string

module Error : sig
  type common = [
    | `Exn of exn
    | `Init_mkdir of [ `Path of string ] * [ `Error of string ]
    | `Empty of 
        [ `Removing of [ `Path of string ] * [ `Error of string ] ]
  ]
  val to_string : 
    [< common
    | `Missing_data of string
    | `Of_json of string ] ->
    string
end

val make : ?gzip_level:int -> string -> t

val update : t -> key -> value ->
  (unit,
   [> `Storage of [> Error.common] ]) Deferred_result.t

val read : t -> key ->
  (value option,
   [> `Storage of [> Error.common] ]) Deferred_result.t

val list : t -> key ->
  (key list,
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
    parse:(Yojson.Safe.json -> [ `Error of 'a | `Ok of 'b ]) ->
    string ->
    ('b, [> `Storage of [> `Exn of exn | `Of_json of 'a ] ]) Deferred_result.t

  val get_json : t -> path:key ->
    parse:(Yojson.Safe.json -> [ `Error of 'a | `Ok of 'b ]) ->
    ('b,
     [> `Storage of
          [> Error.common
          | `Missing_data of string
          | `Of_json of 'a
          ]])
      Deferred_result.t
end

