val command_must_succeed_with_output :
  log:Log.t ->
  ?section:string list ->
  ?additional_json:(string * Yojson.Safe.json) list ->
  string ->
  (string * string,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> Storage.Error.common] ])
    Internal_pervasives.Deferred_result.t

val command_must_succeed :
  log:Log.t ->
  ?section:string list ->
  ?additional_json:(string * Yojson.Safe.json) list ->
  string ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> Storage.Error.common ] ])
    Internal_pervasives.Deferred_result.t
