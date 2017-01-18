open Internal_pervasives

type t =
  | Kube of Kube_cluster.t
[@@deriving yojson, show]

let save ~storage:st cluster =
  Storage.Json.save_jsonable
    st (to_yojson cluster)
    ~path:["cluster"; "default"; "definition.json"]

let get st =
  Storage.Json.get_json
    st ~path:["cluster"; "default"; "definition.json"]
    ~parse:of_yojson

let kube k = Kube k

let max_started_jobs =
  function
  | Kube k -> Kube_cluster.max_started_jobs k

let ensure_living =
  function
  | Kube k -> Kube_cluster.ensure_living k

let do_log_t f ~log =
  function
  | Kube k -> f ~log k
let start ~log t = do_log_t Kube_cluster.gcloud_start ~log t
let delete ~log t = do_log_t Kube_cluster.gcloud_delete ~log t
let describe ~log t = do_log_t Kube_cluster.gcloud_describe ~log t

let display_name =
  function
  | Kube k ->
    sprintf "GCloud-Kube-%s@%s" k.Kube_cluster.name k.Kube_cluster.zone
