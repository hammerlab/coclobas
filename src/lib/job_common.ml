open Internal_pervasives

(** Common tools and function for all [Job] implementations. *)


let job_section id = ["job"; id; "commands"]

let save_path ?tag id =
  let name ?(ext = "json") s =
    sprintf "%s%s.%s"
      s (Option.value_map ~default:"" tag ~f:(sprintf "-%s")) ext in
  function
  | `Saved_state -> ["job"; id; name "saved_state"]
  | `Describe_output -> ["job"; id; name "describe"]
  | `Logs_output -> ["job"; id; name "logs"]

(** The queries [describe] and [logs] have an interesting archival
    mechanism for their results, this module packs results together
    (esp. for display in the Ketrew UI within the plugin). *)
module Query_result = struct
  type job_call_result_item = [
    | `Saved_command of Hyper_shell.Saved_command.t
    | `Url of string
  ]
  [@@deriving yojson, show]
  type t = (string * job_call_result_item) list
  [@@deriving yojson, show]
  type 'a call_function =
    storage:Storage.t ->
    log:Log.t ->
    id:string ->
    (t,
     [> `Log of Log.Error.t
     | `Shell_command of Hyper_shell.Error.t
     | `Storage of [> Storage.Error.common ] ] as 'a) Deferred_result.t

  let one_saved name saved = [name, `Saved_command saved]
  let one_url name saved = [name, `Url saved]
end
