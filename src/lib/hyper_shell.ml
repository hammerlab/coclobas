
open Internal_pervasives




let command_must_succeed_with_output
    ~log ?(section = ["shell-commands"])
    ?(additional_json = [])
    cmd =
  Pvem_lwt_unix.System.Shell.execute cmd
  >>= fun (out, err, ex) ->
  Log.log log ~section
    (`Assoc (
        additional_json
        @ [
          "command", `String cmd;
          "stdout", `String out;
          "stderr", `String err;
          "status", `String (Pvem_lwt_unix.System.Shell.status_to_string ex);
        ]))
  >>= fun () ->
  begin match ex with
  | `Exited 0 -> return (out, err)
  | `Exited _
  | `Signaled _
  | `Stopped _ as e -> fail (`Shell (cmd, e))
  end

let command_must_succeed
    ~log ?section ?additional_json cmd =
  command_must_succeed_with_output
    ~log ?section ?additional_json cmd
  >>= fun (_, _) ->
  return ()
