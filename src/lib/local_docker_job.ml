open Internal_pervasives


module Specification = struct

  type t = {
    image: string;
    command: string list [@main];
    volume_mounts: [ `Local of string * string ] list;
    memory: [ `GB of int | `MB of int ] option;
    cpus: float option;
  } [@@deriving yojson, show, make]
end

let job_section id = ["job"; id; "commands"]

let command_must_succeed ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed ~log cmd ?additional_json
    ~section:(job_section id)
let command_must_succeed_with_output ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd ?additional_json
    ~section:(job_section id)

let exec c =
  List.map c ~f:Filename.quote |> String.concat ~sep:" "

let start ~log ~id ~specification =
  let open Specification in
  let additional_json = [
    "specification", Specification.to_yojson specification;
  ] in
  let mounts =
    specification.volume_mounts |> List.map ~f:(function
        `Local (one, two) -> ["-v"; sprintf "%s:%s" one two]
      )
    |> List.concat in
  let cpus =
    Option.value_map ~default:[] specification.cpus
      ~f:(fun c ->
        (* [sprintf "--cpus=%f" c] -> This option is for Docker ≥ 1.13
           Cf. https://docs.docker.com/engine/admin/resource_constraints/#cpu
        *)
          let period = 1000 in
        [sprintf "--cpu-period=%d" period;
         sprintf "--cpu-quota=%d" (c *. float period |> int_of_float)]) in
  let memory =
    Option.value_map ~default:[] specification.memory
      ~f:(function
        | `GB g -> [sprintf "--memory=%dg" g]
        | `MB m -> [sprintf "--memory=%dm" m]) in
  command_must_succeed ~additional_json ~log ~id
    (["docker"; "run"; "--name"; id; "-d"] 
     @ mounts
     @ cpus
     @ memory
     @ [specification.image]
     @ specification.command
     |> exec)

let describe ~storage ~log ~id =
  let save_path tag = Job_common.save_path ~tag id `Describe_output in
  Hyper_shell.Saved_command.run
    ~storage ~log ~path:(save_path "stats")
    ~section:(job_section id)
    ~keep_the:`Latest
    ~cmd:(["docker"; "stats"; "--no-stream"; id] |> exec)
  >>= fun stat_result ->
  Hyper_shell.Saved_command.run
    ~storage ~log ~path:(save_path "inspect")
    ~section:(job_section id)
    ~keep_the:`Latest
    ~cmd:(["docker"; "inspect"; id] |> exec)
  >>= fun inspect_result ->
  return ["Stats", `Saved_command stat_result;
          "Inspection", `Saved_command inspect_result]

let get_logs ~storage ~log ~id =
  let save_path = Job_common.save_path id `Logs_output in
  Hyper_shell.Saved_command.run
    ~storage ~log ~path:save_path
    ~section:(job_section id)
    ~keep_the:`Latest
    ~cmd:(["docker"; "logs"; id] |> exec)
  >>= fun logres ->
  return (Job_common.Query_result.one_saved "Logs" logres)

let kill ~log ~id =
  command_must_succeed ~log ~id
    (["docker"; "kill"; id] |> exec)


let get_update ~log ~id =
  command_must_succeed_with_output ~log ~id
    (["docker"; "inspect"; id] |> exec)
  >>= fun (stdout, stderr) ->
  Deferred_result.wrap_deferred
    ~on_exn:(fun e -> `Job (`Docker_inspect_json_parsing (stdout, `Exn e)))
    (fun () -> Yojson.Safe.from_string stdout |> Lwt.return)
  >>= fun json ->
  let fail_parsing reason =
    fail (`Job (`Docker_inspect_json_parsing (stdout, `String reason))) in
  begin match json with
  | `List [`Assoc l] ->
    let status = ref None in
    let exit_code = ref None in
    List.iter l ~f:begin function
    | "State", `Assoc json_assoc ->
      List.iter json_assoc ~f:begin function
      | "Status", `String phase ->
        status := Some phase
      | "ExitCode", `Int i ->
        exit_code := Some i
      | _ -> ()
      end
    | _ -> ()
    end;
    begin match !status, !exit_code with
    | Some "running", _ -> return `Running
    | Some "exited", Some 0 -> return `Succeeded
    | Some "exited", Some other -> return `Failed
    | Some other, _ ->
      ksprintf fail_parsing "Unknown State/Status: %s" other
    | None, _ ->
      fail_parsing "Cannot find State/Status in 'inspect' JSON"
    end
  | _ -> fail_parsing "JSON is not an `Assoc _"
  end

module Error = struct
  let to_string =
    function
    | `Docker_inspect_json_parsing (blob, `Exn e) ->
      sprintf "Parsing JSON output of kube-get-pod: %s, %s"
        (Printexc.to_string e)
        blob
    | `Docker_inspect_json_parsing (blob, `String e) ->
      sprintf "Parsing JSON output of kube-get-pod: %s, %s" e blob
end
