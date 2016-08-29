module Error: sig
  type t = {
    command: string [@main];
    stdout: string option;
    stderr: string option;
    status: [`Exited of int | `Signaled of int | `Stopped of int] option;
    exn: string option;
  } [@@deriving yojson,make]
  val to_display_string: t -> string
end

val command_must_succeed_with_output :
  log:Log.t ->
  ?section:string list ->
  ?additional_json:(string * Yojson.Safe.json) list ->
  string ->
  (string * string,
   [> `Shell_command of Error.t
   | `Log of Log.Error.t ])
    Internal_pervasives.Deferred_result.t

val command_must_succeed :
  log:Log.t ->
  ?section:string list ->
  ?additional_json:(string * Yojson.Safe.json) list ->
  string ->
  (unit,
   [> `Shell_command of Error.t
   | `Log of Log.Error.t ])
    Internal_pervasives.Deferred_result.t
