
open Internal_pervasives


module Configuration = struct

  module Default = struct
    let min_sleep = 3.
    let max_sleep = 180.
    let max_update_errors = 10
    let concurrent_steps = 5
  end

  type t = {
    min_sleep: float [@default Default.min_sleep];
    max_sleep: float [@default Default.max_sleep];
    max_update_errors: int [@default Default.max_update_errors];
    concurrent_steps: int [@default Default.concurrent_steps];
  } [@@deriving make, yojson, show]

  let path = ["server"; "configuration.json"]

  let save ~storage conf =
    Storage.Json.save_jsonable storage (to_yojson conf) ~path

  let get st = Storage.Json.get_json st ~path ~parse:of_yojson
end

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
  configuration: Configuration.t;
} [@@deriving make]

let create ~port ~configuration ~root ~cluster ~storage ~log =
  let job_list_mutex = Lwt_mutex.create () in
  let kick_loop = Lwt_condition.create () in
  make ()
    ~job_list_mutex
    ~kick_loop
    ~port ~root ~cluster ~storage ~log
    ~configuration

let log_event t e =
  let json_event name moar_json = 
    `Assoc ([
        "event", `String name;
        "date", `String (ODate.Unix.(now () |> Printer.to_iso));
      ] @ moar_json)
  in
  let stringf fmt = ksprintf (fun s -> `String s) fmt in
  let count l f =
    List.fold l ~init:0 ~f:(fun p k -> p + (if f k then 1 else 0)) in
  let int i = stringf "%d" i in
  let current_jobs () =
    let count_status f = count t.jobs (fun j -> Job.status j |> f) in
    `Assoc [
      "cardinal", List.length t.jobs |> int;
      "submitted", count_status (function `Submitted -> true | _ -> false) |> int;
      "started", count_status (function `Started _ -> true | _ -> false) |> int;
      "errors", count_status (function `Error _ -> true | _ -> false) |> int;
      "finished", count_status (function `Finished _ -> true | _ -> false) |> int;
    ];
  in
  let subsection, json =
    match e with
    | `Ready ->
      "startup",
      json_event "start-with-jobs" [
        "jobs", current_jobs ();
      ]
    | `Loop_begins (started_jobs, todo, batches) ->
      let ctodo = count todo in
      "loop",
      json_event "loop-begins" [
        "batches",
        stringf "[%s]"
          (List.map batches ~f:(fun b -> sprintf "%d" (List.length b))
           |> String.concat ~sep:", ");
        "todo", `Assoc [
          "remove", ctodo (function `Remove _ -> true | _ -> false) |> int;
          "kill", ctodo (function `Kill _ -> true | _ -> false) |> int;
          "start", ctodo (function `Start _ -> true | _ -> false) |> int;
          "update", ctodo (function `Update _ -> true | _ -> false) |> int;
        ];
        "jobs", current_jobs ();
        "started_jobs", int started_jobs;
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
    | `Error (`Storage (`Get_json (_, `Missing_data))) -> return []
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
  begin match Job.Specification.kind spec, Cluster.kind t.cluster with
  | (`Kube, `GCloud_kubernetes) -> return ()
  | (`Local_docker, `Local_docker) -> return ()
  | tuple -> (* we could run local-docker jobs with a kube cluster but that would
            mess with the maximum number of jobs to submit *)
    fail (`Invalid_job_submission (`Wrong_backend tuple))
  end
  >>= fun () ->
  let job = Job.fresh spec in
  Job.save t.storage job
  >>= fun () ->
  change_job_list t (`Add job)
  >>= fun () ->
  Lwt_condition.broadcast t.kick_loop ();
  return (`String (Job.id job))

let batch_list ~max_items l =
  let res = ref [] in
  let rec go l =
    match List.split_n l max_items with
    | some, [] -> res := some :: !res
    | some, more ->
      res := some :: !res;
      go more
  in
  go l;
  List.rev !res

let rec loop:
  ?and_sleep : float -> t -> (unit, _) Deferred_result.t
  = fun ?and_sleep t ->
    let and_sleep =
      Option.value and_sleep ~default:t.configuration.Configuration.min_sleep in
    let now () = Unix.gettimeofday () in
    let `Started started_jobs, todo =
      let currently_started =
        List.fold t.jobs ~init:0 ~f:(fun c j ->
            match Job.status j with
            | `Started _ -> c + 1
            | _ -> c) in
      let max_started = Cluster.max_started_jobs t.cluster in
      List.fold t.jobs ~init:(`Started currently_started, [])
        ~f:(fun (`Started started, todo) j ->
            match Job.status j with
            | `Error _
            | `Finished _ ->
              (`Started started, `Remove j :: todo)
            | other when List.mem ~set:t.jobs_to_kill (Job.id j) ->
              (`Started started, `Kill j :: todo)
            | `Submitted when started >= max_started ->
              (`Started started, todo)
            | `Submitted ->
              (`Started (started + 1), `Start j :: todo)
            | `Started time when time +. 30. > now () ->
              (`Started started, todo)
            | `Started _ ->
              (`Started started, `Update j :: todo)
          )
    in
    t.jobs_to_kill <- [];
    let todo_batches =
      batch_list ~max_items:t.configuration.Configuration.concurrent_steps todo
    in
    log_event t (`Loop_begins (started_jobs, todo, todo_batches))
    >>= fun () ->
    Pvem_lwt_unix.Deferred_list.while_sequential todo_batches ~f:begin fun batch ->
      Pvem_lwt_unix.Deferred_list.for_concurrent batch ~f:begin function
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
            Job.set_status j @@ `Finished (now (), `Killed);
            return ()
          | `Error e ->
            Job.set_status j @@ `Error ("Killing failed: " ^ Error.to_string e);
            return ()
        end
        >>= fun () ->
        Job.save t.storage j
      | `Start j ->
        Job.start ~log:t.log j
        >>< begin function
        | `Ok () -> 
          Job.set_status j @@ `Started (now ());
          Job.save t.storage j
          >>= fun () ->
          return ()
        | `Error e ->
          begin match Job.start_errors j with
          | l when List.length l <= t.configuration.Configuration.max_update_errors ->
            Job.set_start_errors j @@ Error.to_string e :: l;
            Job.save t.storage j
          | more ->
            Job.set_status j @@
              `Error (sprintf
                        "Starting failed %d times: [ %s ]"
                        (t.configuration.Configuration.max_update_errors + 1)
                        (List.dedup more |> String.concat ~sep:" -- "));
            Job.save t.storage j
          end
        end
      | `Update j ->
        begin
          Job.get_update ~log:t.log j
          >>= fun stat ->
          begin match stat with
          | `Running ->
            Job.set_status j @@  `Started (now ());
            Job.save t.storage j
            >>= fun () ->
            return ()
          | (`Failed | `Succeeded as phase) ->
            Job.set_status j @@ `Finished (now (), phase);
            Job.save t.storage j
            >>= fun () ->
            return ()
          end
        end >>< begin function
        | `Ok () -> return ()
        | `Error e ->
          begin match Job.update_errors j with
          | l when List.length l <= t.configuration.Configuration.max_update_errors ->
            Job.set_status j @@ `Started (now ());
            Job.set_update_errors j @@ Error.to_string e :: l;
            Job.save t.storage j
          | more ->
            Job.set_status j @@ 
              `Error (sprintf
                        "Updating failed %d times: [ %s ]"
                        (t.configuration.Configuration.max_update_errors + 1)
                        (List.dedup more |> String.concat ~sep:" -- "));
            Job.save t.storage j
          end
        end
      end
      >>= fun ((_ : unit list), errors) ->
      return errors
    end
    >>= fun 
      (* We make sure only really fatal errors “exit the loop:” *)
      (errors_per_batch : [ `Storage of Storage.Error.common
                          | `Log of Log.Error.t ] list list) ->
    let errors = List.concat errors_per_batch in
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
      let still_some_submitted =
        List.exists t.jobs ~f:(fun j -> Job.status j = `Submitted) in
      match todo, kicked, still_some_submitted with
      | [], false, false ->
        min t.configuration.Configuration.max_sleep (and_sleep *. 2.)
      | _, _, _ -> t.configuration.Configuration.min_sleep in
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

let get_jobs t =
  let jobs = List.map t.jobs (fun j ->
      (`Assoc [
          "id", `String (Job.id j);
          "status", `String (Job.Status.show (Job.status j));
        ])) in
  let json = (`List jobs) in
  return (`Json json)

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
          | "/jobs" ->
            get_jobs t |> respond_result
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
