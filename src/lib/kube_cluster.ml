open Internal_pervasives

type t = {
  name: string [@main];
  zone: string;
  min_nodes: int [@default 1];
  max_nodes: int;
  machine_type: string [@default "n1-highmem-8"]
} [@@deriving yojson, show, make]

let save ~storage:st cluster =
  Storage.Json.save_jsonable
    st (to_yojson cluster)
    ~path:["cluster"; "default"; "definition.json"]

let get st =
  Storage.Json.get_json
    st ~path:["cluster"; "default"; "definition.json"]
    ~parse:of_yojson

let command_must_succeed ~log cluster cmd =
  Hyper_shell.command_must_succeed ~log cmd
    ~section:["cluster"; "commands"]
    ~additional_json:[
      "cluster", to_yojson cluster
    ]

let gcloud_start ~log t =
  let cmd =
    sprintf 
      "gcloud container clusters create %s \
       --quiet --wait \
       --zone %s --num-nodes=%d --min-nodes=%d --max-nodes=%d \
       --machine-type=%s \
       --enable-autoscaling"
      t.name t.zone t.min_nodes t.min_nodes t.max_nodes t.machine_type
  in
  command_must_succeed ~log t cmd

let gcloud_delete ~log t =
  let cmd =
    sprintf 
      "gcloud container clusters delete --quiet --wait %s --zone %s" t.name t.zone in
  command_must_succeed ~log t cmd

let gcloud_describe ~log t =
  let cmd =
    sprintf 
      "gcloud container clusters describe %s --zone %s" t.name t.zone in
  command_must_succeed ~log t cmd

let gcloud_set_current ~log t =
  let cmd =
    sprintf
      "gcloud container clusters get-credentials %s --zone %s" t.name t.zone in
  command_must_succeed ~log t cmd

let ensure_living ~log t =
  gcloud_describe ~log t
  >>< begin function
  | `Ok () ->
    gcloud_set_current ~log t
  | `Error (`Shell (_, `Exited 1)) ->
    gcloud_start ~log t
  | `Error ((`Shell _ | `Storage _) as e) ->
    fail e
  end
