open Internal_pervasives

module Configuration : sig

  module Default : sig
    val min_sleep : float
    val max_sleep : float
    val max_update_errors : int
    val concurrent_steps : int
    val backoff_factor : float
  end

  type t = {
    min_sleep: float [@default Default.min_sleep];
    max_sleep: float [@default Default.max_sleep];
    max_update_errors: int [@default Default.max_update_errors];
    concurrent_steps: int [@default Default.concurrent_steps];
    backoff_factor : float [@default Default.backoff_factor];
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
   | `Aws_batch_queue of [> `Check_valid ] * string * string
   | `Log of Log.Error.t
   | `Storage of [> Storage.Error.common ] ]) Deferred_result.t
