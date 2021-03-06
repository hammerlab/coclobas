open Internal_pervasives

type t = {
  base_url: string [@main];
} [@@deriving yojson, show, make]

let wrap_io d =
  Deferred_result.wrap_deferred d
    ~on_exn:(fun e -> `Client (`IO_exn e))

let wrap_parsing d =
  Deferred_result.wrap_deferred d
    ~on_exn:(fun e -> `Client (`Json_exn e))

let do_get uri =
  wrap_io Lwt.(fun () ->
      Cohttp_lwt_unix.Client.get uri
      >>= fun (resp, body) ->
      Cohttp_lwt_body.to_string body
      >>= fun b ->
      return (resp, b)
    )

let uri_of_ids base_url path ids =
  Uri.with_query
    (Uri.with_path (Uri.of_string base_url) path)
    ["id", ids]

let response_is_ok ~uri ~meth ~body resp =
  begin match Cohttp.Response.status resp with
  | `OK -> return ()
  | other ->
    fail (`Client (`Response (meth, uri, resp, body)))
  end

let submit_job {base_url} spec =
  let uri =
    Uri.with_path (Uri.of_string base_url) "job/submit" in
  let body =
    Cohttp_lwt_body.of_string
      (Job.Specification.to_yojson spec
       |> Yojson.Safe.pretty_to_string)
  in
  wrap_io (fun () -> Cohttp_lwt_unix.Client.post uri ~body)
  >>= fun (resp, ret_body) ->
  wrap_io (fun () -> Cohttp_lwt_body.to_string ret_body)
  >>= fun body ->
  response_is_ok  resp ~meth:`Post ~uri ~body
  >>= fun () ->
  return body

let get_job_jsons {base_url} ~path ~ids  =
  let uri = uri_of_ids base_url path ids in
  do_get uri
  >>= fun (resp, body) ->
  response_is_ok ~body resp ~meth:`Get ~uri
  >>= fun () ->
  wrap_parsing (fun () -> Lwt.return (Yojson.Safe.from_string body))

let get_job_json_one_key t ~path ~ids ~json_key ~of_yojson =
  get_job_jsons t ~path ~ids
  >>= fun json ->
  let uri = uri_of_ids t.base_url path ids in (* Only for error values: *)
  begin match json with
  | `List l ->
    Deferred_list.while_sequential l ~f:(function
      | `Assoc ["id", `String id; key, stjson] when key = json_key ->
        wrap_parsing Lwt.(fun () ->
            let open Ppx_deriving_yojson_runtime.Result in
            match of_yojson stjson with
            | Ok s -> return (id, s)
            | Error e -> fail (Failure e)
          )
      | other -> fail (`Client (`Json_parsing (uri, "Not an Assoc", other)))
      )
  | other -> fail (`Client (`Json_parsing (uri, "Not a List", other)))
  end

let get_job_states t ids =
  get_job_json_one_key t ~path:"job/state" ~ids ~json_key:"state"
    ~of_yojson:Job.of_yojson

let get_json_keys ~uri ~parsers json =
  begin match json with
  | `List l ->
    Deferred_list.while_sequential l ~f:(function
      | `Assoc kv as jkv->
        Deferred_list.while_sequential parsers ~f:(fun (key, of_yojson) ->
            match List.find kv ~f:(fun (k, v) -> k = key) with
            | Some (_, vjson) ->
              wrap_parsing Lwt.(fun () ->
                  match of_yojson vjson with
                  | `Ok s -> return s
                  | `Error e -> fail (Failure e)
                )
            | None ->
              fail (`Client (`Json_parsing (uri, "No key: " ^ key, jkv)))
          )
      | other -> fail (`Client (`Json_parsing (uri, "Not an Assoc", other)))
      )
  | other -> fail (`Client (`Json_parsing (uri, "Not a List", other)))
  end

(* For describe or logs *)
let get_job_query_result ~path t ids =
  get_job_jsons t ~path ~ids
  >>= fun json ->
  let uri = uri_of_ids t.base_url path ids in (* Only for error values: *)
  begin match json with
  | `List l ->
    Deferred_list.while_sequential l ~f:(function
      | `Assoc ["id", `String id;
                "output", yoj] ->
        begin match (Job_common.Query_result.of_yojson yoj) with
        | Ok output ->
          return (id, output)
        | Error e ->
          fail (`Client (`Json_parsing
                           (uri, "Not an query-result", yoj)))
        end
      | other ->
        fail (`Client (`Json_parsing
                         (uri, "Not an {id: ... output: ...}", other)))
      )
  | other -> fail (`Client (`Json_parsing (uri, "Not a List", other)))
  end

let get_job_descriptions t ids =
  get_job_query_result ~path:"job/describe" t ids

let get_job_logs t ids =
  get_job_query_result ~path:"job/logs" t ids

let kill_jobs {base_url} ids =
  let uri = uri_of_ids base_url "job/kill" ids in
  do_get uri
  >>= fun (resp, body) ->
  response_is_ok resp ~body ~meth:`Get ~uri

let get_server_status_string {base_url} =
  let uri = Uri.with_path (Uri.of_string base_url) "status" in
  do_get uri
  >>= fun (resp, body) ->
  response_is_ok resp ~body ~meth:`Get ~uri
  >>= fun () ->
  return body

let get_cluster_description {base_url} =
  let uri = Uri.with_path (Uri.of_string base_url) "cluster/describe" in
  do_get uri
  >>= fun (resp, body) ->
  response_is_ok resp ~body ~meth:`Get ~uri
  >>= fun () ->
  return body

let get_job_list {base_url} =
  let uri = Uri.with_path (Uri.of_string base_url) "jobs" in
  do_get uri
  >>= fun (resp, body) ->
  let json = Yojson.Safe.from_string body in
  let get_string name =
    function
    | `String i -> `Ok i
    | other -> `Error (sprintf "%s not a string" name)
  in
  get_json_keys ~uri json ~parsers:[
    "id", get_string "status";
    "status", get_string "status";
  ]
  >>= fun (res : string list list) ->
  Deferred_list.while_sequential res ~f:(
    function
    | [id; status] ->
      return (`Id id, `Status status)
    | other ->
      ksprintf failwith
        "This should never happen: 2 parsers  Vs %d results: [%s]"
        (List.length other)
        (String.concat ~sep:", " other)
  )

module Error = struct
  let to_string =
    function
    | `IO_exn e -> sprintf "Client.IO: %s" (Printexc.to_string e)
    | `Json_exn e -> sprintf "Client.Json-parsing: %s" (Printexc.to_string e)
    | `Json_parsing (uri, problem, json) ->
      sprintf "Client.Json-parsing: URI: %s, problem: %s, content: %s"
        (Uri.to_string uri)
        problem
        (Yojson.Safe.pretty_to_string json)
    | `Response (meth, uri, resp, body) ->
      sprintf "Client.Response: URI: %s, Meth: %s, Resp: %s, Body: %s"
        (Uri.to_string uri)
        begin match meth with
        | `Post -> "POST"
        | `Get -> "GET"
        end
        (Cohttp.Response.sexp_of_t resp |> Sexplib.Sexp.to_string_hum)
        body
end
