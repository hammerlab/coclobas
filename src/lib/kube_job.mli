open Internal_pervasives

module Specification : sig
  module Nfs_mount : sig
    type t = private {
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

  type t = private {
    id : string;
    image : string;
    command : string list;
    volume_mounts : [ `Nfs of Nfs_mount.t ] list;
    memory : [ `GB of int ];
    cpus : int;
  }
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> [ `Error of string | `Ok of t ]
  val show : t -> Ppx_deriving_runtime.string

  val make :
    id:string ->
    image:string ->
    ?command:string list ->
    ?volume_mounts:[ `Nfs of Nfs_mount.t ] list ->
    ?memory:[ `GB of int ] -> ?cpus:int -> unit -> t

  val fresh :
    image:string ->
    ?volume_mounts:[ `Nfs of Nfs_mount.t ] list -> string list -> t
end

module Status : sig
  type t = [
    | `Error of string
    | `Finished of float * [ `Failed | `Succeeded ]
    | `Started of float
    | `Submitted
  ]
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> [ `Error of string | `Ok of t ]

  val show : t -> Ppx_deriving_runtime.string
end

type t = { specification : Specification.t; mutable status : Status.t; }

val show : t -> Ppx_deriving_runtime.string
val make : ?status:Status.t -> Specification.t -> t

val id : t -> string

val status : t -> Status.t

val save :
  Storage.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val get :
  Storage.t ->
  string ->
  (t,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of
        [> `Exn of exn | `Missing_data of string | `Of_json of string ] ])
    Deferred_result.t

val start :
  log:Log.t ->
  t ->
  (unit,
   [> `IO of [> `Write_file_exn of Pvem_lwt_unix.IO.path * exn ]
   | `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val describe :
  log:Log.t ->
  t ->
  (string,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val kill :
  log:Log.t ->
  t ->
  (unit,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

val get_status_json :
  log:Log.t ->
  t ->
  (string,
   [> `Shell of
        string *
        [> `Exited of int
        | `Exn of exn
        | `Signaled of int
        | `Stopped of int ]
   | `Storage of [> `Exn of exn ] ]) Deferred_result.t

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
