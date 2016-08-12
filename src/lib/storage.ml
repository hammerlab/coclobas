open Internal_pervasives

module Store =
  Irmin_unix.Irmin_git.FS
    (Irmin.Contents.String)
    (Irmin.Ref.String)
    (Irmin.Hash.SHA1)

type key = Store.key
type value = Store.value
type t = {
  root: string;
  mutable store: (string -> Store.t) option
}

let make root = {root; store = None}

let wrap lwt =
  Deferred_result.wrap_deferred lwt ~on_exn:(fun e -> `Storage (`Exn e))

let init {root; _} =
  ksprintf Pvem_lwt_unix.System.Shell.do_or_fail "mkdir -p %s" root
  >>= fun () ->
  let config = Irmin_unix.Irmin_git.config ~root ~bare:true () in
  wrap (fun () -> Store.Repo.create config)
  >>= fun repo ->
  wrap (fun () -> Store.master Irmin_unix.task repo)

let get_store t msg =
  match t.store with
  | Some f -> return (f msg)
  | None ->
    init t
    >>= fun store ->
    t.store <- Some store;
    return (store msg)

let update t k v =
  let msg = sprintf "Update /%s" (String.concat ~sep:"/" k) in
  print_endline msg;
  get_store t msg
  >>= fun s ->
  wrap (fun () -> Store.update s k v)

let read t k =
  let msg = sprintf "Read /%s" (String.concat ~sep:"/" k) in
  print_endline msg;
  get_store t msg
  >>= fun s ->
  wrap (fun () -> Store.read s k)

let list t k =
  let msg = sprintf "List /%s" (String.concat ~sep:"/" k) in
  print_endline msg;
  get_store t msg
  >>= fun s ->
  wrap (fun () -> Store.list s k)

module Json = struct
  let of_yojson_error = function
  | `Ok o -> return o
  | `Error s -> fail (`Storage (`Of_json s))

  let save_jsonable st ~path yo =
    let json = yo |> Yojson.Safe.pretty_to_string ~std:true in
    update st path json

  let parse_json_blob ~parse json =
    wrap (fun () -> Lwt.return (Yojson.Safe.from_string json))
    >>= fun yo ->
    of_yojson_error (parse yo)

  let get_json st ~path ~parse =
    read st path
    >>= begin function
    | Some json -> parse_json_blob ~parse json
    | None -> fail (`Storage (`Missing_data (String.concat ~sep:"/" path)))
    end
end

module Error = struct
  let to_string =
    function
    | `Exn _ as e -> Generic_error.to_string e
    | `Of_json s -> s
    | `Missing_data s -> sprintf "Missing data: %s" s
end

