

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ();;
#use "topfind"
#thread
#require "ketrew"
open Nonstd
open Ketrew.Configuration
let debug_level = 2

let engine =
  let database_parameters =
    "postgresql://127.0.0.1/?user=postgres&password=kpass"
    (* :"/tmp/ketrew/database" *)
  in
  engine ~database_parameters ()

let env_exn s =
  try Sys.getenv s with _ -> ksprintf failwith "Missing environment variable: %S" s

let port =
  env_exn "PORT" |> Int.of_string
  |> Option.value_exn ~msg:"$PORT is not an integer"

let token = (env_exn "AUTH_TOKEN")
let server =
  server ~engine
    ~authorized_tokens:[
      authorized_token ~name:"From-env" token;
     ]
    ~return_error_messages:true
    ~log_path:"/tmp/ketrew/logs/"
    ~command_pipe:"/tmp/ketrew/command.pipe"
    (`Tcp port)

let client =
  client ~token (sprintf "http://127.0.0.1:%d" port)

let () =
  output [
    profile "server" (create ~debug_level (server));
    profile "client" (create ~debug_level (client));
  ]
