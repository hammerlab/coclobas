
open Coclobas
open Internal_pervasives

let name = "coclobas-kube"

module Run_parameters = struct

  type t = [
    | `Created of Client.t * Kube_job.Specification.t
    | `Running of Client.t * Kube_job.Specification.t * string
  ] [@@deriving yojson, show]

  let serialize run_parameters =
    to_yojson run_parameters
    |> Yojson.Safe.pretty_to_string ~std:true

  let deserialize_exn s =
    Yojson.Safe.from_string s |> of_yojson
    |> function
    | `Ok o -> o
    | `Error e -> failwith e
end

let create ~base_url spec = 
  `Long_running (name, `Created (Client.make base_url, spec) |> Run_parameters.serialize)

let run_program ~base_url ~image ?(volume_mounts = []) p =
  let script_path = "/coclo-kube/mount/script" in
  let script =
    Kube_job.Specification.File_contents_mount.fresh
      ~path:script_path
      Ketrew_pure.Monitored_script.(
        create p
          ~playground:(Ketrew_pure.Path.absolute_directory_exn
                         "/tmp/playground")
        |> to_string
      ) in
  let spec =
    Kube_job.Specification.make
      ~image
      ~volume_mounts:(`Constant script :: volume_mounts)
      ["bash"; script_path]
  in
  create ~base_url spec


module Long_running_implementation : Ketrew.Long_running.LONG_RUNNING = struct

  type run_parameters = Run_parameters.t
  include Run_parameters
     
  let name = "coclobas-kube"

  module KLRU = Ketrew.Long_running_utilities

  let serialize run_parameters =
    Run_parameters.to_yojson run_parameters
    |> Yojson.Safe.pretty_to_string ~std:true

  let deserialize_exn s =
    Yojson.Safe.from_string s |> Run_parameters.of_yojson
    |> function
    | `Ok o -> o
    | `Error e -> failwith e

  let classify_client_error m =
    m >>< function
    | `Ok o -> return o
    | `Error (`Client cl) -> fail (`Fatal (Client.Error.to_string cl))
    | `Error (`Coclo_plugin (`Expecting_one_status l)) ->
      fail (`Fatal (sprintf "Expecting one status but got: [%s]"
                      (List.map l ~f:(fun (id, st) -> id)
                       |> String.concat ~sep:"; ")))

  let start :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    (run_parameters, Ketrew.Host_io.Error.classified) Deferred_result.t =
    fun rp ~host_io ->
      match rp with
      | `Running _ ->
        ksprintf KLRU.fail_fatal "start on already running: %s"
          (Run_parameters.show rp)
      | `Created (client, spec) ->
        classify_client_error begin
          Client.submit_kube_job client spec
          >>= fun id ->
          return (`Running (client, spec, id))
        end

  let running rp f =
    match rp with
    | `Created _ ->
      ksprintf KLRU.fail_fatal "update on not started: %s"
        (Run_parameters.show rp)
    | `Running (client, spec, id) ->
      f id client spec

  let update :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    ([ `Failed of run_parameters * string
     | `Still_running of run_parameters
     | `Succeeded of run_parameters ], Ketrew.Host_io.Error.classified)
      Deferred_result.t =
    fun rp ~host_io ->
      running rp begin fun job_id client spec ->
        classify_client_error begin
          Client.get_kube_job_statuses client [job_id]
          >>= fun statuses ->
          begin match statuses with
          | [(_, st)] ->
            begin match st with
            | `Finished (_,`Succeeded) ->
              return (`Succeeded rp)
            | `Error e ->
              return (`Failed (rp, sprintf "Error-from-coclobas: %s" e))
            | `Finished (_,`Failed) ->
              return (`Failed (rp, "job-failed"))
            | `Submitted
            | `Started _ -> return (`Still_running rp)
            end
          | other ->
            fail (`Coclo_plugin (`Expecting_one_status other))
          end
        end
      end

  let kill :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    ([ `Killed of run_parameters ], Ketrew.Host_io.Error.classified)
      Deferred_result.t =
    fun rp ~host_io ->
      running rp begin fun id client spec ->
        classify_client_error begin
          Client.kill_kube_jobs client [id]
          >>= fun () ->
          return (`Killed rp)
        end
      end

  let log :
    run_parameters -> (string * Ketrew_pure.Internal_pervasives.Log.t) list =
    fun rp ->
      let open Ketrew_pure.Internal_pervasives.Log in
      let common (client, job) = [
        "Client", Coclobas.Client.show client |> s;
        "Job", Coclobas.Kube_job.Specification.show job |> s;
      ] in
      match rp with
      | `Created c ->
        ("status", s "Created") :: common c
      | `Running (cli, spec, id) ->
        ("status", s "Running")
        :: ("id", s id)
        :: common (cli, spec)

  let additional_queries :
    run_parameters -> (string * Ketrew_pure.Internal_pervasives.Log.t) list =
    fun rp ->
      let open Ketrew_pure.Internal_pervasives.Log in
      let common = [
        "display", s "Display the contents of the run-parameters";
        "server-status", s "Get the server status";
      ] in
      match rp with
      | `Created c -> common
      | `Running c ->
        (
          ("job-status", s "Get the “raw” job status")
          :: ("kubectl-describe", s "Get the `describe` blob from Kubernetes")
          :: ("kubectl-logs", s "Get the `logs` blob from Kubernetes")
          :: common
        )


  let client_query m =
    m >>< function
    | `Ok o -> return o
    | `Error (`Client ce) ->
      fail (Ketrew_pure.Internal_pervasives.Log.verbatim (Client.Error.to_string ce))

  let query :
    run_parameters ->
    host_io:Ketrew.Host_io.t ->
    string ->
    (string, Ketrew_pure.Internal_pervasives.Log.t) Deferred_result.t =
    fun rp ~host_io query ->
      let open Ketrew_pure.Internal_pervasives.Log in
      let (`Created (client, jobspec) | `Running (client, jobspec, _)) = rp in
      match query, rp with
      | "display", _ ->
        return (
          log rp
          |> List.map ~f:(fun (k, v) ->
              sprintf "- %s: %s"
                k
                (to_long_string v))
          |> String.concat ~sep:"\n"
        )
      | "server-status", _ ->
        client_query begin
          Coclobas.Client.get_server_status_string client
        end
      | "job-status", `Running (_, _, id) ->
        client_query begin
          Client.get_kube_job_statuses client
            [id]
          >>= fun l ->
          let rendered =
            List.map l ~f:(fun (id, s) ->
                sprintf "* %s: %s" id (Kube_job.Status.show s))
            |> String.concat ~sep:"\n"
          in
          return rendered
        end
      | "kubectl-describe" , `Running (_, _, id) ->
        client_query begin
          Client.get_kube_job_descriptions client [id]
          >>= fun l ->
          let rendered =
            List.map l ~f:(fun (id, s) ->
                sprintf "### Kube-Job %s\n\n%s" id  s)
            |> String.concat ~sep:"\n"
          in
          return rendered
        end
      | "kubectl-logs", `Running (_, _, id) ->
        client_query begin
          Client.get_kube_job_logs client [id]
          >>= fun l ->
          let rendered =
            List.map l ~f:(fun (id, s) ->
                sprintf "### Kube-Job %s\n\n%s" id  s)
            |> String.concat ~sep:"\n"
          in
          return rendered
        end
      | other, _ -> fail (s "Unknown query: " % s other)

end
let () =
  Ketrew.Plugin.register_long_running_plugin ~name (module Long_running_implementation)

include Long_running_implementation
