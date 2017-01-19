open Internal_pervasives


module Specification = struct

  type t = {
    image: string;
    command: string list [@main];
    volume_mounts: [ `Local of string * string ] list;
  } [@@deriving yojson, show, make]
end

let command_must_succeed ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed ~log cmd ?additional_json
    ~section:["job"; id; "commands"]
let command_must_succeed_with_output ~log ?additional_json ~id cmd =
  Hyper_shell.command_must_succeed_with_output ~log cmd ?additional_json
    ~section:["job"; id; "commands"]
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
  command_must_succeed ~additional_json ~log ~id
    (["docker"; "run"; "--name"; id; "-d"] 
     @ mounts
     @ [specification.image]
     @ specification.command
     |> exec)

let always_fresh (out, err) =
  (`Fresh, out ^ err)

let describe ~log ~id =
  command_must_succeed_with_output ~log ~id
    (["docker"; "stats"; "--no-stream"; id] |> exec)
  >>= fun (statout, staterr) ->
  command_must_succeed_with_output ~log ~id
    (["docker"; "inspect"; id] |> exec)
  >>= fun (inspout, insperr) ->
  return (always_fresh
            (sprintf "#### Stats:\n%s\n\n\
                      #### Inspection:\n%s\n\n\
                      #### Errors:\n%s\n%s\n"
               statout inspout staterr insperr, ""))

let get_logs ~log ~id =
  command_must_succeed_with_output ~log ~id
    (["docker"; "logs"; id] |> exec)
  >>| always_fresh

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
