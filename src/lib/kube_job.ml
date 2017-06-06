open Internal_pervasives

module Specification = struct
  module Nfs_mount = struct
    type t = {
      host: string;
      path: string;
      point: string;
      read_only: bool [@default false];
    } [@@deriving yojson, show, make]
    let id m =
      let sanitize_dns s =
        String.map s ~f:begin
          function
          | ('a' .. 'z' | '0' .. '9' | 'A' .. 'Z') as c -> c
          | other -> '-'
        end
        |> fun s ->
        begin match s.[0] with
        | Some ('a' .. 'z') -> s
        | None -> "nohost"
        | other -> "host-" ^ s
        end
      in
      Hashtbl.hash m |> sprintf "%s-%x" (sanitize_dns m.host)
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

let command_must_succeed ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed ~log cmd ?additional_json
    ~section:(Job_common.job_section id)
let command_must_succeed_with_output ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd ?additional_json
    ~section:(Job_common.job_section id)

let start ~log ~id ~specification =
  let spec = specification in
  let open Specification in
  let secret_name f =
    String.take
      (id ^ (Digest.string (File_contents_mount.show f) |> Digest.to_hex))
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
            "name", `String id;
            "labels", `Assoc [
              "app", `String id;
            ];
          ];
          "spec", `Assoc [
            "restartPolicy", `String "Never";
            "containers", `List [
              `Assoc [
                "name", `String (id ^ "container");
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
    (command_must_succeed ~additional_json ~log ~id)
    "kubectl create -f %s" tmp


let describe ~storage ~log ~id =
  let cmd = sprintf "kubectl describe pod %s" id in
  let save_path = Job_common.save_path id `Describe_output in
  Hyper_shell.Saved_command.run
    ~storage ~log ~cmd ~path:save_path
    ~section:(Job_common.job_section id)
    ~keep_the:`Latest
  >>= fun logres ->
  return (Job_common.Query_result.one_saved "Description" logres)

let get_logs ~storage ~log ~id =
  let save_path = Job_common.save_path id `Logs_output in
  let cmd = sprintf "kubectl logs %s" id in
  Hyper_shell.Saved_command.run
    ~storage ~log ~cmd ~path:save_path
    ~section:(Job_common.job_section id)
    ~keep_the:`Largest
  >>= fun logres ->
  return (Job_common.Query_result.one_saved "Logs" logres)


let kill ~log ~id =
  let cmd = sprintf "kubectl delete pod %s" id in
  command_must_succeed ~log ~id cmd

let get_status_json ~log ~id =
  let cmd = sprintf "kubectl get pod -a %s -o=json" id in
  command_must_succeed_with_output ~log ~id cmd
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
