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
    (t,
     [> `Storage of
          [> Storage.Error.common
          | `Missing_data of string
          | `Of_json of string ] ])
      Deferred_result.t
end


type t

val create :
  port:int ->
  configuration: Configuration.t ->
  root:string ->
  cluster:Kube_cluster.t ->
  storage:Storage.t ->
  log:Log.t ->
  t

val start: t ->
  (unit,
   [> `Shell_command of Hyper_shell.Error.t
   | `Log of Log.Error.t
   | `Storage of [> Storage.Error.common
                 | `Missing_data of string
                 | `Of_json of string ] ]) Deferred_result.t
