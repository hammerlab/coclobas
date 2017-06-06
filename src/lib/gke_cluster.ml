open Internal_pervasives

type t = {
  name: string [@main];
  zone: string;
  min_nodes: int [@default 1];
  max_nodes: int;
  machine_type: string [@default "n1-highmem-8"];
  image_type: [ `Set of string | `Default] [@default (`Set "container_vm")];
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

let command_must_succeed_with_output ~log cluster cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd
    ~section:["cluster"; "commands"]
    ~additional_json:[
      "cluster", to_yojson cluster
    ]

let max_started_jobs cluster =
  cluster.max_nodes + 5

let gcloud_start ~log t =
  let cmd =
    let image_type_option =
      match t.image_type with
      | `Default -> ""
      | `Set i -> sprintf "--image-type=%s" i
    in
    sprintf 
      "gcloud container clusters create %s %s \
       --quiet \
       --zone %s --num-nodes=%d --min-nodes=%d --max-nodes=%d \
       --machine-type=%s \
       --enable-autoscaling"
      t.name image_type_option
      t.zone t.min_nodes t.min_nodes t.max_nodes
      t.machine_type
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
  command_must_succeed_with_output ~log t cmd

let gcloud_set_current ~log t =
  let cmd =
    sprintf
      "gcloud container clusters get-credentials %s --zone %s" t.name t.zone in
  command_must_succeed ~log t cmd

let ensure_living ~log t =
  gcloud_describe ~log t
  >>< begin function
  | `Ok _ ->
    gcloud_set_current ~log t
  | `Error (`Shell_command error)
    when error.Hyper_shell.Error.status = Some (`Exited 1) ->
    gcloud_start ~log t
  | `Error ((`Shell_command _ | `Log _) as e) ->
    fail e
  end
