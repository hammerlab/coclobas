open Internal_pervasives

type stored = {
  storage: Storage.t;
}
type t = [
  | `Silent
  | `Stored of stored
]

let silent : t = `Silent
let stored storage : t = `Stored {storage}

let log ?(section = ["main"]) t json =
  match t with
  | `Silent -> return ()
  | `Stored st ->
    let name =
      let now = Unix.gettimeofday () in
      sprintf "%s_%s"
        (truncate (1000. *. now) |> Int.to_string)
        (Hashtbl.hash json |> sprintf "%x")
    in
    let path = "logs" :: section @ [name ^ ".json"] in
    Storage.Json.save_jsonable st.storage json ~path
