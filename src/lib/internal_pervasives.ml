

include Nonstd
module String = Sosa.Native_string

include Pvem_lwt_unix
let (>>=) = Deferred_result.(>>=)
let (>><) = Deferred_result.(>><)
let (>>|) = Deferred_result.(>>|)
let return = Deferred_result.return
let fail = Deferred_result.fail
               

let dbg fmt =
  ksprintf (fun s -> printf "Cocldebug>> %s\n%!" s) fmt


module Generic_error = struct
  let to_string =
    function
    | `Exn e -> Printexc.to_string e
end

