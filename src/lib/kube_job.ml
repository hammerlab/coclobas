open Internal_pervasives

module Specification = struct
  module Nfs_mount = struct
    type t = {
      host: string;
      path: string;
      point: string;
      read_only: bool [@default false];
    }
      [@@deriving yojson, show, make]
    let id m = Hashtbl.hash m |> sprintf "%s-%x" m.host
    let point m = m.point
    let host m = m.host
    let path m = m.path
    let point m = m.point
    let read_only m = m.read_only
  end
  module File_contents_mount = struct
    type t = {
      path: string;
      contents: string [@main];
    } [@@deriving yojson, show, make]
    let path t = t.path
    let contents t = t.contents
  end
  type t = {
    image: string;
    command: string list [@main];
    volume_mounts: [ `Nfs of Nfs_mount.t | `Constant of File_contents_mount.t ] list;
    memory: [ `GB of int ] [@default `GB 50];
    cpus: int [@default 7];
  } [@@deriving yojson, show, make]
end
module Status = struct
  type t = [
    | `Submitted
    | `Started of float
    | `Finished of float * [ `Failed | `Succeeded | `Killed ]
    | `Error of string
  ] [@@deriving yojson,show ] 
end

type t = {
  id: string;
  specification: Specification.t [@main];
  mutable status: Status.t [@default `Submitted];
  mutable update_errors : string list;
} [@@deriving yojson, show, make]

let fresh spec =
    let id = Uuidm.(v5 (create `V4) "coclojobs" |> to_string ~upper:false) in
    make ~id spec

let id t = t.id

let status t = t.status

let make_path id =
  function
  | `Specification -> ["job"; id; "specification.json"]
  | `Status -> ["job"; id; "status.json"]
  | `Describe_output -> ["job"; id; "describe.out"]
  | `Logs_output -> ["job"; id; "logs.out"]

let save st job =
  Storage.Json.save_jsonable st
    ~path:(make_path (id job) `Specification)
    (Specification.to_yojson job.specification)
  >>= fun () ->
  Storage.Json.save_jsonable st
    ~path:(make_path (id job) `Status)
    (Status.to_yojson job.status)

let get st job_id =
  Storage.Json.get_json st
    ~path:(make_path job_id `Specification)
    ~parse:Specification.of_yojson
  >>= fun specification ->
  Storage.Json.get_json st
    ~path:(make_path job_id `Status)
    ~parse:Status.of_yojson
  >>= fun status ->
  return {id = job_id; specification; status; update_errors = []}

let command_must_succeed ~log ?additional_json job cmd =
  Hyper_shell.command_must_succeed ~log cmd ?additional_json
    ~section:["job"; id job; "commands"]
let command_must_succeed_with_output ~log ?additional_json job cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd ?additional_json
    ~section:["job"; id job; "commands"]

let start ~log t =
  let spec = t.specification in
  let open Specification in
  let secret_name f =
    String.take
      (t.id ^ (Digest.string (File_contents_mount.show f) |> Digest.to_hex))
      55 (* The max is 63 + we want to add "-volume" *)
  in
  let requests_json =
    `Assoc [
      "memory", (let `GB gb = spec.memory in `String (sprintf "%dG" gb));
      "cpu", `String (Int.to_string spec.cpus);
    ] in
  let json : Yojson.Safe.json =
    let secrets =
      List.filter_map spec.volume_mounts ~f:(function
        | `Nfs _ -> None
        | `Constant f ->
          Some  (`Assoc [
              "apiVersion", `String "v1";
              "kind", `String "Secret";
              "metadata", `Assoc [
                "name", `String (secret_name f);
              ];
              "data", `Assoc [
                Filename.basename (File_contents_mount.path f),
                `String (File_contents_mount.contents f |> B64.encode);
              ];
            ] )) in
    let items =
      secrets
      @ [
        `Assoc [
          "apiVersion", `String "v1";
          "kind", `String "Pod";
          "metadata", `Assoc [
            "name", `String t.id;
            "labels", `Assoc [
              "app", `String t.id;
            ];
          ];
          "spec", `Assoc [
            "restartPolicy", `String "Never";
            "containers", `List [
              `Assoc [
                "name", `String (t.id ^ "container");
                "image", `String spec.image;
                "command", `List (List.map spec.command ~f:(fun s -> `String s));
                "volumeMounts",
                `List (
                  List.map spec.volume_mounts ~f:(function
                    | `Constant f ->
                      `Assoc [
                        "name", `String (secret_name f ^ "-volume");
                        "readOnly", `Bool true;
                        "mountPath", `String (Filename.dirname
                                                (File_contents_mount.path f));
                      ]
                    | `Nfs m ->
                      `Assoc [
                        "name", `String (Nfs_mount.id m);
                        "mountPath", `String (Nfs_mount.point m);
                      ])
                );
                "resources", `Assoc [
                  "requests", requests_json;
                ];
              ];
            ];
            "volumes", `List (
              List.map spec.volume_mounts ~f:(function
                | `Constant f ->
                  `Assoc [
                    "name", `String (secret_name f ^ "-volume");
                    "secret", `Assoc [
                      "secretName",  `String (secret_name f);
                    ]
                  ]   
                | `Nfs m ->
                  `Assoc [
                    "name", `String (Nfs_mount.id m);
                    "nfs", `Assoc [
                      "server", `String (Nfs_mount.host m);
                      "path", `String (Nfs_mount.path m);
                      "readOnly", `Bool (Nfs_mount.read_only m);
                    ];
                  ])
            );
          ];
        ];
      ]
    in
    `Assoc [
      "apiVersion", `String "v1";
      "kind", `String "List";
      "items", `List items;
    ]
  in
  let json_string = Yojson.Safe.pretty_to_string ~std:true json in
  let tmp = Filename.temp_file "coclojob" ".json" in
  Pvem_lwt_unix.IO.write_file tmp ~content:json_string
  >>= fun () ->
  let additional_json = [
    "temp-file", `String tmp;
    "contents", json;
  ] in
  ksprintf
    (command_must_succeed ~additional_json ~log t)
    "kubectl create -f %s" tmp

let save_command ~storage ~log t ~cmd ~path_kind =
  begin
    command_must_succeed_with_output ~log t cmd
    >>< function
    | `Ok (out, err) ->
      Storage.update storage (make_path (id t) path_kind) (out ^ err)
      >>= fun () ->
      return (`Fresh, out)
    | `Error (`Shell_command _ as e) ->
      Storage.read storage (make_path (id t) path_kind)
      >>= begin function
      | Some old -> return (`Archived e, old)
      | None -> fail e
      end
    | `Error e -> fail e
  end

let describe ~storage ~log t =
  let cmd = sprintf "kubectl describe pod %s" (id t) in
  save_command ~storage ~log t ~cmd ~path_kind:`Describe_output

let get_logs ~storage ~log t =
  let cmd = sprintf "kubectl logs %s" (id t) in
  save_command ~storage ~log t ~cmd ~path_kind:`Logs_output


let kill ~log t =
  let cmd = sprintf "kubectl delete pod %s" t.id in
  command_must_succeed ~log t cmd

let get_status_json ~log t =
  let cmd = sprintf "kubectl get pod %s -o=json" (id t) in
  command_must_succeed_with_output ~log t cmd
  >>= fun (out, _) ->
  return out

module Kube_status = struct
  (* cf. http://kubernetes.io/docs/user-guide/pod-states/ *)
  type t = {
    phase : [ `Pending | `Running | `Succeeded | `Failed | `Unknown ];
  }
    [@@deriving show,yojson]

  let phase_of_string =
    function
    | "Pending"   -> Some `Pending
    | "Running"   -> Some `Running
    | "Succeeded" -> Some `Succeeded
    | "Failed"    -> Some `Failed
    | "Unknown"   -> Some `Unknown
    | other -> None

  let of_json blob =    
    Deferred_result.wrap_deferred
      ~on_exn:(fun e -> `Job (`Kube_json_parsing (blob, `Exn e)))
      (fun () -> Yojson.Safe.from_string blob |> Lwt.return)
    >>= fun json ->
    let fail_parsing reason =
      fail (`Job (`Kube_json_parsing (blob, `String reason))) in
    begin match json with
    | `Assoc l ->
      let phase =
        List.find_map l ~f:(function
          | "status", `Assoc json_assoc ->
            List.find_map json_assoc ~f:(function
              | "phase", `String phase -> Some phase
              | _ -> None)
          | _ -> None)
        |> Option.bind ~f:phase_of_string
      in
      begin match phase with
      | None -> fail_parsing "Cannot find field /status/phase"
      | Some phase -> return {phase}
      end
    | _ -> fail_parsing "JSON is not an `Assoc _"
    end
end

module Error = struct
  let to_string =
    function
    | `Kube_json_parsing (blob, `Exn e) ->
      sprintf "Parsing JSON output of kube-get-pod: %s, %s"
        (Printexc.to_string e)
        blob
    | `Kube_json_parsing (blob, `String e) ->
      sprintf "Parsing JSON output of kube-get-pod: %s, %s" e blob
end
