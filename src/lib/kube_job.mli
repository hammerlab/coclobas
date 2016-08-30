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

module Status : sig
  type t = [
    | `Error of string
    | `Finished of float * [ `Failed | `Succeeded | `Killed ]
    | `Started of float
    | `Submitted
  ]
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> [ `Error of string | `Ok of t ]

  val show : t -> Ppx_deriving_runtime.string
end

type t = {
  id: string;
  specification : Specification.t;
  mutable status : Status.t; }

val fresh : Specification.t -> t

val show : t -> Ppx_deriving_runtime.string

val id : t -> string

val status : t -> Status.t

val save :
  Storage.t ->
  t ->
  (unit,
   [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t

val get :
  Storage.t ->
  string ->
  (t,
   [> `Storage of
        [> Storage.Error.common | `Missing_data of string | `Of_json of string ] ])
    Deferred_result.t

val start :
  log:Log.t ->
  t ->
  (unit,
   [> `IO of [> `Write_file_exn of Pvem_lwt_unix.IO.path * exn ]
   | `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val describe :
  log:Log.t ->
  t ->
  (string,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val kill :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val get_logs:
  log:Log.t ->
  t ->
  (string * string,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

val get_status_json :
  log:Log.t ->
  t ->
  (string,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t ]) Deferred_result.t

module Kube_status : sig
  type t = {
    phase : [ `Failed | `Pending | `Running | `Succeeded | `Unknown ];
  }
  val show : t -> Ppx_deriving_runtime.string

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> [ `Error of string | `Ok of t ]

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
