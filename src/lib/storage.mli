open Internal_pervasives

type t
type key = string list
type value = string

val make : string -> t

val update : t -> key -> value ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ])
  Deferred_result.t

val read : t -> key ->
  (value option,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ])
  Deferred_result.t

val list : t -> key ->
  (key list,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ])
    Deferred_result.t

module Json : sig

  val save_jsonable : t -> path:key -> Yojson.Safe.json ->
    (unit,
     [> `Shell of
          string *
          [> `Exited of int
          | `Exn of exn
          | `Signaled of int
          | `Stopped of int ]
     | `Storage of [> `Exn of exn ] ])
    Deferred_result.t

  val parse_json_blob :
    parse:(Yojson.Safe.json -> [ `Error of 'a | `Ok of 'b ]) ->
    string ->
    ('b, [> `Storage of [> `Exn of exn | `Of_json of 'a ] ]) Deferred_result.t

  val get_json : t -> path:key ->
    parse:(Yojson.Safe.json -> [ `Error of 'a | `Ok of 'b ]) ->
    ('b,
     [> `Shell of
          string *
          [> `Exited of int
          | `Exn of exn
          | `Signaled of int
          | `Stopped of int ]
     | `Storage of
          [> `Exn of exn | `Missing_data of string | `Of_json of 'a ] ])
    Deferred_result.t
end

module Error : sig
  val to_string :
    [< `Exn of exn | `Missing_data of string | `Of_json of string ] ->
    string
end
