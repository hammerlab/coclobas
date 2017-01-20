open Internal_pervasives

module Specification : sig
  module Nfs_mount :
  sig
    type t = {
      host : string;
      path : string;
      point : string;
      read_only : bool;
    }
    val show : t -> Ppx_deriving_runtime.string
    val make :
      host:string ->
      path:string -> point:string -> ?read_only:bool -> unit -> t
    val id : t -> string
    val host : t -> string
    val path : t -> string
    val point : t -> string
    val read_only : t -> bool
  end
  module File_contents_mount : sig
    type t = { path : string; contents : string; }
    val show : t -> Ppx_deriving_runtime.string
    val make : path:string -> string -> t
    val path : t -> string
    val contents : t -> string
  end
  type t = {
    image: string;
    command: string list [@main];
    volume_mounts: [ `Nfs of Nfs_mount.t | `Constant of File_contents_mount.t ] list;
    memory: [ `GB of int ] [@default `GB 50];
    cpus: int [@default 7];
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
  storage:Storage.t ->
  log:Log.t ->
  id:string ->
  save_path:Storage.key ->
  ([ `Fresh | `Archived of [ `Shell_command of Hyper_shell.Error.t ] ] * string,
   [> `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t
   | `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val kill :
  log:Log.t ->
  id: string ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val get_logs:
  storage:Storage.t ->
  log:Log.t ->
  id: string ->
  save_path: Storage.key ->
  ([ `Fresh | `Archived of [ `Shell_command of Hyper_shell.Error.t ] ] * string,
   [> `Log of Log.Error.t
   | `Shell_command of Hyper_shell.Error.t
   | `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val get_status_json :
  log:Log.t ->
  id: string ->
  (string,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

module Kube_status : sig
  type t = {
    phase : [ `Failed | `Pending | `Running | `Succeeded | `Unknown ];
  } [@@deriving yojson, show]

  val phase_of_string :
    string ->
    [> `Failed | `Pending | `Running | `Succeeded | `Unknown ] option

  val of_json :
    string ->
    (t,
     [> `Job of
          [> `Kube_json_parsing of
               string * [> `Exn of exn | `String of string ] ] ]) Deferred_result.t
end


module Error : sig
  val to_string :
    [< `Kube_json_parsing of
         string * [< `Exn of exn | `String of string ] ] ->
    string
end
