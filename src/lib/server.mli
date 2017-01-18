open Internal_pervasives

module Configuration : sig

  module Default : sig
    val min_sleep : float
    val max_sleep : float
    val max_update_errors : int
    val concurrent_steps : int
  end

  type t = {
    min_sleep: float [@default Default.min_sleep];
    max_sleep: float [@default Default.max_sleep];
    max_update_errors: int [@default Default.max_update_errors];
    concurrent_steps: int [@default Default.concurrent_steps];
  } [@@deriving make, yojson, show]

  val save :
    storage:Storage.t ->
    t ->
    (unit, [> `Storage of [> Storage.Error.common ] ])
      Deferred_result.t
  val get :
    Storage.t ->
    (t, [> `Storage of [> Storage.Error.common] ]) Deferred_result.t
end

module Cluster: sig
  type t
  val kube: Kube_cluster.t -> t
  val display_name: t -> string
  val save :
    storage:Storage.t ->
    t ->
    (unit,
     [> `Storage of [> Storage.Error.common ] ]) Deferred_result.t
  val get :
    Storage.t ->
    (t,
     [>`Storage of [> Storage.Error.common ] ])
      Deferred_result.t
  val start :
    log:Log.t ->
    t ->
    (unit,
     [> `Shell_command of Hyper_shell.Error.t
     | `Log of Log.Error.t ]) Deferred_result.t
  val delete :
    log:Log.t ->
    t ->
    (unit,
     [> `Shell_command of Hyper_shell.Error.t
     | `Log of Log.Error.t ]) Deferred_result.t
  val describe :
    log:Log.t ->
    t ->
    (string * string,
     [> `Shell_command of Hyper_shell.Error.t
     | `Log of Log.Error.t ]) Deferred_result.t

end

type t

val create :
  port:int ->
  configuration: Configuration.t ->
  root:string ->
  cluster:Cluster.t ->
  storage:Storage.t ->
  log:Log.t ->
  t

val start: t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t
   | `Storage of [> Storage.Error.common ] ]) Deferred_result.t
