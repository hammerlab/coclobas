module Error: sig
  type t = {
    command: string [@main];
    stdout: string option;
    stderr: string option;
    status: [`Exited of int | `Signaled of int | `Stopped of int] option;
    exn: string option;
  } [@@deriving yojson,make,show]
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

module Saved_command : sig
  module Output_archive : sig
    type t = {
      date: float;
      out: string;
      err: string;
    } [@@deriving yojson,show,make]
    val to_string: t -> string
  end
  type t = {
    command: string;
    outcome: [
      | `Ok of string * string
      | `Error of Error.t
    ];
    archived: Output_archive.t option;
  } [@@deriving yojson,show,make]
  val run :
    storage:Storage.t ->
    log:Log.t ->
    section:string list ->
    cmd:string ->
    path:Storage.key ->
    keep_the:[ `Largest | `Latest ] ->
    (t, [> `Log of Log.Error.t | `Storage of [> Storage.Error.common ] ])
      Internal_pervasives.Deferred_result.t
end