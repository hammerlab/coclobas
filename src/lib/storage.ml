open Internal_pervasives

type key = string list
type value = string

let key_of_path = String.concat ~sep:"/"

type t = {
  parameters: string;
  mutable handle: Trakeva_of_uri.t option;
  store_mutex: Lwt_mutex.t;
  collection: string;
}

let make parameters =
  {
    parameters; handle = None;
    store_mutex = Lwt_mutex.create ();
    collection = "coclobas";
  }

module Error = struct
  type where = [
    | `Update of key
    | `Read of key
    | `Parsing_json of string
  ]
  type common = [
    | `Exn of where * exn
    | `Backend of where * string
    | `Of_json of where * string
    | `Get_json of where * [ `Missing_data ]
  ]
  let to_string e =
    let where w =
      match w with
      | `Update u -> sprintf "update: %s" (key_of_path u)
      | `Read r -> sprintf "read: %s" (key_of_path r)
      | `Parsing_json js ->
        sprintf "Parsing json: %s (%d B)"
          (String.sub js ~index:0 ~length:30 |> Option.value ~default:js)
          (String.length js)
    in
    sprintf "Storage error: %s"
      begin match e with
      | `Exn (wh, e) -> sprintf "%s: %s" (where wh) (Printexc.to_string e)
      | `Backend (wh, s) -> sprintf "%s: Trakeva: %s" (where wh) s
      | `Of_json (wh, s) -> sprintf "%s: From-Yojson: %s" (where wh) s
      | `Get_json (wh, `Missing_data) ->
        sprintf "Get-json: %s: missing data!" (where wh)
      end

  let wrap_trakeva ~info m =
    m >>< function
    | `Ok o -> return o
    | `Error (`Database trakeva) ->
      fail (`Storage (`Backend (info, Trakeva.Error.to_string trakeva)))
    
end
let wrap ~info lwt =
  Deferred_result.wrap_deferred lwt ~on_exn:(fun e -> `Storage (`Exn (info, e)))

let init t =
  Trakeva_of_uri.load t.parameters

let get_store_no_mutex t =
  match t.handle with
  | Some h -> return h
  | None ->
    init t
    >>= fun store ->
    t.handle <- Some store;
    return store

let on_store t ~f =
  Lwt_mutex.with_lock t.store_mutex begin fun () ->
    get_store_no_mutex t
    >>= fun s ->
    f s
  end

let update t k v : (unit, [> `Storage of [> Error.common] ] ) Deferred_result.t =
  on_store t ~f:(fun s ->
      let action =
        Trakeva.Action.(set ~collection:t.collection ~key:(key_of_path k) v) in
      Trakeva_of_uri.act s ~action
      >>= begin function
      | `Done -> return ()
      | `Not_done ->
        dbg "NOT DONNEEE???? %s %s" (key_of_path k) v;
        failwith "not-done"
      end
    )
  |> Error.wrap_trakeva ~info:(`Update k)

let read t k =
  on_store t ~f:(fun s ->
      Trakeva_of_uri.get s ~collection:t.collection ~key:(key_of_path k))
  |> Error.wrap_trakeva ~info:(`Read k)

let empty t =
  return ()


module Json = struct
  let of_yojson_error ~info =
    let open Ppx_deriving_yojson_runtime.Result in
    function
    | Ok o -> return o
    | Error s -> fail (`Storage (`Of_json (info, s)))

  let save_jsonable st ~path yo =
    let json = yo |> Yojson.Safe.pretty_to_string ~std:true in
    update st path json

  let parse_json_blob ~parse json =
    let info = (`Parsing_json json) in
    wrap ~info (fun () -> Lwt.return (Yojson.Safe.from_string json))
    >>= fun yo ->
    of_yojson_error ~info (parse yo)

  let get_json st ~path ~parse =
    read st path
    >>= begin function
    | Some json -> parse_json_blob ~parse json
    | None ->
      fail (`Storage (`Get_json (`Read path, `Missing_data)))
    end
end


