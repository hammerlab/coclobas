
open Internal_pervasives

module Error = struct

  type t = {
    command: string [@main];
    stdout: string option;
    stderr: string option;
    status: [`Exited of int | `Signaled of int | `Stopped of int] option;
    exn: string option;
  } [@@deriving yojson,make,show]

  let of_result cmd =
    function
    | `Ok (stdout, stderr, status) ->
      make ~stdout ~stderr ~status cmd
    | `Error (`Shell (_, `Exn e)) ->
      make cmd ~exn:(Printexc.to_string e)

  let to_display_string t =
    let show_output name =
      function
      | "" -> sprintf "%s: empty.\n" name
      | more ->
        sprintf "%s:\n```\n%s%s```\n" name more
          String.(if get more (length more - 1) = Some '\n' then "" else "\n")
    in
    sprintf "Command:\n```\n%s\n```\n%s"
      t.command
      (List.filter_opt
         [
           Option.map t.status
             ~f:(fun s ->
                 sprintf
                   "Status: %s\n" (Pvem_lwt_unix.System.Shell.status_to_string s));
           Option.map t.exn ~f:(sprintf "Exception: %s\n");
           Option.map t.stdout ~f:(show_output "Standard-output");
           Option.map t.stderr ~f:(show_output "Standard-error");
         ]
       |> String.concat ~sep:"\n")
end



let command_must_succeed_with_output
    ~log ?(section = ["shell-commands"])
    ?(additional_json = [])
    cmd =
  Pvem_lwt_unix.System.Shell.execute cmd
  >>< begin fun res ->
    let error = Error.of_result cmd res in
    Log.log log ~section
      (`Assoc (
          additional_json
          @ [
            "command", Error.to_yojson error;
          ]))
    >>= fun () ->
    match res with
    | `Ok (out, err, ex) ->
      begin match ex with
      | `Exited 0 -> return (out, err)
      | `Exited _
      | `Signaled _
      | `Stopped _ -> fail (`Shell_command error)
      end
    | `Error _ -> fail (`Shell_command error)
  end

let command_must_succeed
    ~log ?section ?additional_json cmd =
  command_must_succeed_with_output
    ~log ?section ?additional_json cmd
  >>= fun (_, _) ->
  return ()


module Saved_command = struct
  type t = {
    command: string;
    outcome: [
      | `Ok of string * string
      | `Error of Error.t
    ];
    archived: string option;
  } [@@deriving yojson,show,make]


  let run ~storage ~log ~section ~cmd ~path ~keep_the =
    begin
      command_must_succeed_with_output ~log ~section cmd
      >>< function
      | `Ok (out, err) ->
        let new_output = out ^ err in
        begin match keep_the with
        | `Latest ->
          Storage.update storage path new_output
        | `Largest ->
          Storage.read storage path
          >>= begin function
          | Some old when String.length old > String.length new_output ->
            return ()
          | Some _ (* smaller *) | None ->
            Storage.update storage path new_output
          end
        end
        >>= fun () ->
        Storage.read storage path
        >>= fun archived ->
        return (make
                  ~command:cmd ~outcome:(`Ok (out, err))
                  ?archived ())
      | `Error (`Shell_command e) ->
        Storage.read storage path
        >>= fun archived ->
        return (make
                  ~command:cmd ~outcome:(`Error e) ?archived ())
      | `Error (`Log _ as e) -> fail e
    end
end
