
open Internal_pervasives

module Specification : sig
  type t = {
    image: string;
    command: string list [@main];
    volume_mounts: [ `Local of string * string ] list;
  } [@@deriving yojson, show, make]
end

val start :
  log:Log.t ->
  id:string ->
  specification:Specification.t ->
  (unit,
   [> `IO of [> `Write_file_exn of Pvem_lwt_unix.IO.path * exn ]
   | `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val describe :
  _ Job_common.Query_result.call_function

val get_logs:
  _ Job_common.Query_result.call_function

val kill :
  log:Log.t ->
  id: string ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val get_update :
  log: Log.t ->
  id: string ->
  ([> `Failed | `Running | `Succeeded ],
   [> `Job of
        [> `Docker_inspect_json_parsing of
             string * [> `Exn of exn | `String of string ]
        ]
   | `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t ]) Deferred_result.t

module Error : sig
  val to_string :
    [< `Docker_inspect_json_parsing of
         string * [< `Exn of exn | `String of string ] ] -> string
end
