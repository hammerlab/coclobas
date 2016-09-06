
open Internal_pervasives

module Cluster = Kube_cluster

module Job = Kube_job

type t = {
  port : int;
  mutable status: [ `Initializing | `Ready ] [@default `Initializing];
  root : string;
  mutable cluster : Cluster.t;
  mutable jobs: Job.t list;
  mutable jobs_to_kill: string list;
  storage: Storage.t;
  log: Log.t;
  job_list_mutex: Lwt_mutex.t;
  kick_loop: unit Lwt_condition.t;
} [@@deriving make]

let create ~port ~root ~cluster ~storage ~log =
  let job_list_mutex = Lwt_mutex.create () in
  let kick_loop = Lwt_condition.create () in
  make ()
    ~job_list_mutex
    ~kick_loop
    ~port ~root ~cluster ~storage ~log

let log_event t e =
  let json_event name moar_json = 
    `Assoc ([
        "event", `String name;
        "date", `String (ODate.Unix.(now () |> Printer.to_iso));
      ] @ moar_json)
  in
  let stringf fmt = ksprintf (fun s -> `String s) fmt in
  let current_jobs () =
    `List (List.map t.jobs ~f:(fun j ->
        `Assoc [
          "id", `String (Job.id j);
          "status", (Job.status j |> Job.Status.to_yojson);
        ])) in
  let subsection, json =
    match e with
    | `Ready ->
      "startup",
      json_event "start-with-jobs" [
        "jobs", current_jobs ();
      ]
    | `Loop_begins todo ->
      "loop",
      json_event "loop-begins" [
        "todo", `List 
          (List.map todo ~f:(function
             | `Remove j -> stringf "rm %s" (Job.id j)
             | `Kill j -> stringf "kill %s" (Job.id j)
             | `Start j -> stringf "start %s" (Job.id j)
             | `Update j -> stringf "update %s" (Job.id j)));
        "jobs", current_jobs ();
      ]
    | `Loop_ends (sleep, errors) ->
      "loop",
      json_event "loop-ends" [
        "sleep", `Float sleep;
        "errors", `List (List.map errors ~f:(fun x ->
            `String (Error.to_string x)));
        "jobs", current_jobs ();
      ]
    | `Changed_job_list action ->
      let action =
        match action with
        | `Add j ->
          `Assoc ["verb", `String "add"; "job", `String (Job.id j)]
        | `Remove j ->
          `Assoc ["verb", `String "remove"; "job", `String (Job.id j)]
      in
      "job-list",
      json_event "changed-job-list" [
        "action", action;
        "jobs", current_jobs ();
      ]
  in
  Log.log t.log ~section:["server"; subsection] json

let change_job_list t action =
  Lwt_mutex.with_lock t.job_list_mutex begin fun () ->
    begin match action with
    | `Add j -> t.jobs <- j :: t.jobs
    | `Remove j ->
      t.jobs <- List.filter t.jobs ~f:(fun jj -> Job.id jj <> Job.id j);
    end;
    Storage.Json.save_jsonable t.storage
      ~path:["server"; "jobs.json"]
      (`List (List.map t.jobs ~f:(fun j -> `String (Job.id j))))
    >>= fun () ->
    log_event t (`Changed_job_list action)
  end

let get_job_list t =
  let parse =
    let open Ppx_deriving_yojson_runtime in
    let open Ppx_deriving_yojson_runtime.Result in
    function
    | `List l ->
      Nonstd.List.fold ~init:(Ok []) l ~f:(fun prev j ->
          prev >>= fun l ->
          match j with
          | `String s -> Ok (s :: l)
          | other -> Error "expecting List of Strings")
    | other -> Error "expecting List (of Strings)"
  in
  begin
    Storage.Json.get_json t.storage ~path:["server"; "jobs.json"] ~parse
    >>< function
    | `Ok ids -> return ids
    | `Error (`Storage (`Missing_data md)) -> return []
    | `Error other -> fail other
  end
  >>= fun ids ->
  Deferred_list.while_sequential ids ~f:(fun id ->
      Job.get t.storage id)
  >>= fun jobs ->
  t.jobs <- jobs;
  return ()

let incoming_job t string =
  Storage.Json.parse_json_blob ~parse:Job.Specification.of_yojson string
  >>= fun spec ->
  let job = Job.fresh spec in
  Job.save t.storage job
  >>= fun () ->
  change_job_list t (`Add job)
  >>= fun () ->
  Lwt_condition.broadcast t.kick_loop ();
  return (`String (Job.id job))

let min_sleep = 3.
let max_sleep = 180.
let max_update_errors = 10

let rec loop:
  ?and_sleep : float -> t -> (unit, _) Deferred_result.t
  = fun ?(and_sleep = min_sleep) t ->
    let now () = Unix.gettimeofday () in
    let todo =
      List.fold t.jobs ~init:[] ~f:(fun prev j ->
          match Job.status j with
          | `Error _
          | `Finished _ -> `Remove j :: prev
          | other when List.mem ~set:t.jobs_to_kill (Job.id j) ->
            `Kill j :: prev
          | `Submitted -> `Start j :: prev
          | `Started time when time +. 30. > now () -> prev
          | `Started _ -> `Update j :: prev
        )
    in
    t.jobs_to_kill <- [];
    log_event t (`Loop_begins (todo))
    >>= fun () ->
    Pvem_lwt_unix.Deferred_list.for_sequential todo ~f:begin function
    | `Remove j ->
      (* We call these functions once to give them a chance to save the output
         before Kubernetes forgets about the job: *)
      (Job.get_logs ~storage:t.storage ~log:t.log j >>< fun _ -> return ())
      >>= fun () ->
      (Job.describe ~storage:t.storage ~log:t.log j >>< fun _ -> return ())
      >>= fun () ->
      change_job_list t (`Remove j)
    | `Kill j ->
      begin
        Job.kill ~log:t.log j
        >>< function
        | `Ok () ->
          j.Job.status <- `Finished (now (), `Killed);
          return ()
        | `Error e ->
          j.Job.status <- `Error ("Killing failed: " ^ Error.to_string e);
          return ()
      end
      >>= fun () ->
      Job.save t.storage j
    | `Start j ->
      Job.start ~log:t.log j
      >>< begin function
      | `Ok () -> 
        j.Job.status <- `Started (now ());
        Job.save t.storage j
        >>= fun () ->
        return ()
      | `Error e ->
        j.Job.status <- `Error ("Starting failed: " ^ Error.to_string e);
        Job.save t.storage j
        >>= fun () ->
        return ()
      end
    | `Update j ->
      begin
        Job.get_status_json ~log:t.log j
        >>= fun blob ->
        Job.Kube_status.of_json blob
        >>= fun stat ->
        let open Job.Kube_status in
        begin match stat with
        | { phase = `Pending }
        | { phase = `Unknown }
        | { phase = `Running } ->
          j.Job.status <- `Started (now ());
          Job.save t.storage j
          >>= fun () ->
          return ()
        | { phase = (`Failed | `Succeeded as phase)} ->
          j.Job.status <- `Finished (now (), phase);
          Job.save t.storage j
          >>= fun () ->
          return ()
        end
      end >>< begin function
      | `Ok () -> return ()
      | `Error e ->
        begin match j.Job.update_errors with
        | l when List.length l <= max_update_errors ->
          j.Job.status <- `Started (now ());
          j.Job.update_errors <- Error.to_string e :: l;
          Job.save t.storage j
        | more ->
          j.Job.status <-
            `Error (sprintf
                      "Updating failed %d times: [ %s ]"
                      (max_update_errors + 1)
                      (List.dedup more |> String.concat ~sep:" -- "));
          Job.save t.storage j
        end
      end
    end
    >>= fun ((_ : unit list),
             (* We make sure only really fatal errors “exit the loop:” *)
             (errors : [ `Storage of Storage.Error.common
                       | `Log of Log.Error.t ] list)) ->
    log_event t (`Loop_ends (and_sleep, errors))
    >>= fun () ->
    begin match errors with
    | [] -> return ()
    | _ :: _ ->
      dbg "%s → ERRORS IN THE LOOP!!!: %s"
        ODate.Unix.(now () |> Printer.to_iso)
        (List.map errors ~f:Error.to_string |> String.concat ~sep:"\n");
      exit 5
    end
    >>= fun () ->
    Deferred_list.pick_and_cancel [
      (Pvem_lwt_unix.System.sleep and_sleep >>< fun _ -> return false);
      Lwt.(Lwt_condition.wait t.kick_loop >>= fun () -> return (`Ok true));
    ]
    >>= fun kicked ->
    let and_sleep =
      match todo, kicked with
      | [], false -> min max_sleep (and_sleep *. 2.)
      | _, _ -> min_sleep in
    loop ~and_sleep t

let initialization t =
  Cluster.ensure_living ~log:t.log t.cluster
  >>= fun () ->
  get_job_list t
  >>= fun () ->
  t.status <- `Ready;
  log_event t `Ready;
  >>= fun () ->
  Lwt.async (fun () -> loop t);
  return ()

let get_job_status t ids =
  Deferred_list.while_sequential ids ~f:(fun id ->
      Job.get t.storage id
      >>= fun job ->
      return (`Assoc [
          "id", `String id;
          "status", Job.status job |> Job.Status.to_yojson;
        ]))
  >>= fun l ->
  return (`Json (`List l))

let make_json_of_freshness_result ~freshness ~id ~key ~value =
  let frstr =
    match freshness with
    | `Fresh -> "Fresh"
    | `Archived e -> sprintf "Archived because of error: %s" (Error.to_string e)
  in
  (`Assoc [
      "id", `String id;
      key, `String value;
      "freshness", `String frstr;
    ])

let get_job_logs t ids =
  Deferred_list.while_sequential ids ~f:(fun id ->
      Job.get t.storage id
      >>= fun job ->
      Job.get_logs ~storage:t.storage ~log:t.log job
      >>= fun (freshness, value) ->
      return (make_json_of_freshness_result
                ~id ~freshness ~key:"output" ~value))
  >>= fun l ->
  return (`Json (`List l))


let get_job_description t ids =
  Deferred_list.while_sequential ids ~f:(fun id ->
      Job.get t.storage id
      >>= fun job ->
      Job.describe ~storage:t.storage ~log:t.log job
      >>= fun (freshness, value) ->
      return (make_json_of_freshness_result
                ~id ~freshness ~key:"description" ~value))
  >>= fun l ->
  return (`Json (`List l))


let kill_jobs t ids =
  t.jobs_to_kill <- ids @ t.jobs_to_kill;
  Lwt_condition.broadcast t.kick_loop ();
  return `Done



let respond_result r =
  let open Cohttp in
  let module Coserver = Cohttp_lwt_unix.Server in
  let open Lwt in
  r >>= begin function
  | `Ok `Done -> Coserver.respond_string ~status:`OK ~body:"Done" ()
  | `Ok (`String body) -> Coserver.respond_string ~status:`OK ~body ()
  | `Ok (`Json j) ->
    let body = Yojson.Safe.pretty_to_string ~std:true j in
    Coserver.respond_string ~status:`OK ~body ()
  | `Error e ->
    Coserver.respond_string
      ~status:`Bad_request ~body:(Error.to_string e) ()
  end

let job_ids_of_uri uri =
  Uri.query uri
  |> List.concat_map ~f:(function | ("id", l) -> l | _ -> [])

let empty_logs t =
  Log.empty t.log
  >>= fun () ->
  return `Done

let start t =
  let condition = Lwt_condition.create () in
  let server_thread () =
    Deferred_result.wrap_deferred
      ~on_exn:(fun e -> `Start_server (`Exn e))
      begin fun () ->
        let open Cohttp in
        let module Coserver = Cohttp_lwt_unix.Server in
        let open Lwt in
        let callback _conn req body =
          let uri = req |> Request.uri in
          match Uri.path uri with
          | "/status" ->
            let body =
              match t.status with
              | `Initializing -> "Initializing"
              | `Ready -> "Ready"
            in
            Coserver.respond_string ~status:`OK ~body ()
          | "/kick" ->
            Lwt_condition.broadcast t.kick_loop ();
            respond_result (return (`Ok `Done))
          | "/empty-logs" ->
            empty_logs t |> respond_result
          | "/job/status" ->
            get_job_status t (job_ids_of_uri uri) |> respond_result
          | "/job/logs" ->
            get_job_logs t (job_ids_of_uri uri) |> respond_result
          | "/job/describe" ->
            get_job_description t (job_ids_of_uri uri) |> respond_result
          | "/job/kill" ->
            kill_jobs t  (job_ids_of_uri uri) |> respond_result
          | "/job/submit" ->
            body |> Cohttp_lwt_body.to_string
            >>= fun body_string ->
            incoming_job t body_string
            |> respond_result
          | other ->
            let meth = req |> Request.meth |> Code.string_of_method in
            let headers = req |> Request.headers |> Header.to_string in
            body |> Cohttp_lwt_body.to_string >|= (fun body ->
                (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
                   (Uri.to_string uri) meth headers body))
            >>= (fun body -> Coserver.respond_string ~status:`OK ~body ())
        in
        Coserver.create ~mode:(`TCP (`Port t.port)) (Coserver.make ~callback ())
        >>= fun () ->
        Lwt_condition.signal condition (`Ok ());
        return ()
      end
  in
  Lwt.async server_thread;
  initialization t
  >>= fun () ->
  Lwt_condition.wait condition
