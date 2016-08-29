open Internal_pervasives

type stored = {
  storage: Storage.t;
}
type t = [
  | `Silent
  | `Stored of stored
  | `File_tree of string (* The root *)
]

let silent : t = `Silent
let stored storage : t = `Stored {storage}
let file_tree root : t = `File_tree root

let debug_sections : string list list ref = ref []
let () =
  try debug_sections :=
      Sys.getenv "COCLOBAS_DEBUG_SECTIONS"
      |> String.split ~on:(`Character ',')
      |> List.map ~f:(fun p ->
          String.split ~on:(`Character '/') p |> List.filter ~f:((<>) ""))
  with _ -> ()

module Error = struct
  type t =
    [ `IO of [ `Write_file_exn of string * exn ]
    | `System of
        [ `File_info of string
        | `List_directory of string
        | `Make_directory of string
        | `Remove of string ] *
        [ `Exn of exn | `Wrong_access_rights of int ]
    | `Storage of  Storage.Error.common 
    ]
  let to_string =
    function
    | `IO _ as e -> Pvem_lwt_unix.IO.error_to_string e
    | `System _ as s -> Pvem_lwt_unix.System.error_to_string s
    | `Storage s -> Storage.Error.to_string s

  let wrap m =
    m >>< function `Ok o -> return o | `Error e -> fail (`Log e)
end

let empty l =
  Error.wrap begin
    match l with
    | `Silent -> return ()
    | `Stored s -> Storage.empty s.storage
    | `File_tree root ->
      let open Pvem_lwt_unix in
      System.remove root
      >>= fun () ->
      System.ensure_directory_path root
  end

let log ?(section = ["main"]) t json =
  Error.wrap begin
    let path =
      let name =
        let now = Unix.gettimeofday () in
        sprintf "%s_%s"
          (truncate (1000. *. now) |> Int.to_string)
          (Hashtbl.hash json |> sprintf "%x")
      in
      "logs" :: section @ [name ^ ".json"] in
    begin match List.mem section ~set:!debug_sections with
    | true ->
      printf "<<<< Coclobas.Log %s (%s)\n%s\n>>>>\n%!"
        ODate.Unix.(now () |> Printer.to_iso)
        (String.concat ~sep:"/" path)
        (Yojson.Safe.pretty_to_string json ~std:true);
    | false -> ()
    end;
    match t with
    | `Silent -> return ()
    | `File_tree root ->
      let path_str = String.concat ~sep:"/" (root :: path) in
      Pvem_lwt_unix.System.ensure_directory_path (Filename.dirname path_str)
      >>= fun () ->
      let content = Yojson.Safe.pretty_to_string json ~std:true in
      IO.write_file path_str ~content
    | `Stored st ->
      Storage.Json.save_jsonable st.storage json ~path
  end
