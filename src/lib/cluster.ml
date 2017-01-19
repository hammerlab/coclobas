open Internal_pervasives

type local_docker =
  {max_jobs: int}
[@@deriving yojson, show]

type t =
  | Kube of Kube_cluster.t
  | Local_docker of local_docker
[@@deriving yojson, show]

let kind =
  function
  | Kube _ -> `GCloud_kubernetes
  | Local_docker _ -> `Local_docker

let save ~storage:st cluster =
  Storage.Json.save_jsonable
    st (to_yojson cluster)
    ~path:["cluster"; "default"; "definition.json"]

let get st =
  Storage.Json.get_json
    st ~path:["cluster"; "default"; "definition.json"]
    ~parse:of_yojson

let kube k = Kube k

let local_docker ~max_jobs = Local_docker {max_jobs}

let max_started_jobs =
  function
  | Kube k -> Kube_cluster.max_started_jobs k
  | Local_docker { max_jobs } -> max_jobs

let ensure_living t ~log =
  match t with
  | Kube k -> Kube_cluster.ensure_living ~log k
  | Local_docker _ ->
    (* TODO: check docker exists and is ready *)
    return ()

let display_name =
  function
  | Kube k ->
    sprintf "GCloud-Kube-%s@%s" k.Kube_cluster.name k.Kube_cluster.zone
  | Local_docker { max_jobs } -> sprintf "Localdocker_Max-%d" max_jobs

let do_log_t f ~log =
  function
  | Kube k -> f ~log k
  | Local_docker _ -> return ()

let start ~log t =
  do_log_t Kube_cluster.gcloud_start ~log t

let delete ~log t = do_log_t Kube_cluster.gcloud_delete ~log t

let describe ~log =
  function
  | Kube k -> Kube_cluster.gcloud_describe ~log k
  | Local_docker _ as t -> return (display_name t, "")

