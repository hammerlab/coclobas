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

let debug_sections : string list list ref = ref []
let () =
  try debug_sections :=
      Sys.getenv "COCLOBAS_DEBUG_SECTIONS"
      |> String.split ~on:(`Character ',')
      |> List.map ~f:(fun p ->
          String.split ~on:(`Character '/') p |> List.filter ~f:((<>) ""))
  with _ -> ()

let empty =
  function
  | `Silent -> return ()
  | `Stored s -> Storage.empty s.storage

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
    begin match List.mem section ~set:!debug_sections with
    | true ->
      printf "<<<< Coclobas.Log %s (%s)\n%s\n>>>>\n%!"
        ODate.Unix.(now () |> Printer.to_iso)
        (String.concat ~sep:"/" path)
        (Yojson.Safe.pretty_to_string json ~std:true);
    | false -> ()
    end;
    Storage.Json.save_jsonable st.storage json ~path
