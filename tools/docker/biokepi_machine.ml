
#use "topfind";;
#thread
#require "coclobas.ketrew_backend,biokepi";;

open Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat

let env_exn s =
  try Sys.getenv s with _ ->
    ksprintf failwith "Missing environment variable %S" s

let work_dir =
  env_exn "BIOKEPI_WORK_DIR"

let install_tools_path =
  try env_exn "INSTALL_TOOLS_PATH"
  with _ -> work_dir // "toolkit"

let pyensembl_cache_dir =
  try env_exn "PYENSEMBLE_CACHE_DIR"
  with _ -> work_dir // "pyensembl-cache"

let reference_genomes_path =
  try env_exn "REFERENCE_GENOME_PATH"
  with _ -> work_dir // "reference-genome"

let allow_daemonize =
  try env_exn "ALLOW_DAEMONIZE" = "true"
  with _ -> false

let image =
  try env_exn "DOCKER_IMAGE"
  with _ -> "hammerlab/biokepi-run"

let env_exn_tool_loc s tool =
  try (`Wget (Sys.getenv s)) with _ ->
    `Fail (sprintf "No location provided for %s" tool)

let gatk_jar_location () = env_exn_tool_loc "GATK_JAR_URL" "GATK"
let mutect_jar_location () = env_exn_tool_loc "MUTECT_JAR_URL" "MuTect"
let netmhc_tool_locations () = Biokepi.Setup.Netmhc.({
  netmhc=env_exn_tool_loc "NETMHC_TARBALL_URL" "NetMHC";
  netmhcpan=env_exn_tool_loc "NETMHCPAN_TARBALL_URL" "NetMHCpan";
  pickpocket=env_exn_tool_loc "PICKPOCKET_TARBALL_URL" "PickPocket";
  netmhccons=env_exn_tool_loc "NETMHCCONS_TARBALL_URL" "NetMHCcons";
})

let volume_mounts =
  env_exn "NFS_MOUNTS"
  |> String.split ~on:(`Character ':')
  |> List.map ~f:(fun csv ->
      String.split ~on:(`Character ',') csv
      |> begin function
      | host :: path :: witness :: point :: [] ->
        `Nfs (
          Coclobas.Kube_job.Specification.Nfs_mount.make
            ~host ~path ~point ())
      | other ->
        ksprintf failwith "Wrong format for NFS_MOUNTS: %S" csv
      end)

let name = "Coclomachine"

let run_program_with_coclo p =
  let open Coclobas in
  let open Coclobas_ketrew_backend.Plugin in
  let base_url = "http://127.0.0.1:8082" in
  let script_path = "/coclo-kube/mount/script" in
  let script =
    Kube_job.Specification.File_contents_mount.make
      ~path:script_path
      Ketrew_pure.Monitored_script.(
        create p
          ~playground:(Ketrew_pure.Path.absolute_directory_exn
                         "/tmp/playground")
        |> to_string
      ) in
  let get_int_option envvar ~f =
    match env_exn envvar |> Int.of_string with
    | Some i -> Some (f i)
    | None ->
      ksprintf failwith "Variable %S should be an integer not %S"
        envvar (env_exn envvar)
    | exception _ -> None in
  let memory = get_int_option "KUBE_JOB_MEMORY" ~f:(fun gb -> `GB gb) in
  let cpus = get_int_option "KUBE_JOB_CPUS" ~f:(fun c -> c)  in
  let spec =
    Kube_job.Specification.make
      ?memory ?cpus
      ~image
      ~volume_mounts:(`Constant script :: volume_mounts)
      ["bash"; script_path]
  in
  let created =
    Run_parameters.make_created ~client:(Client.make base_url)
      ~program:p spec in
  `Long_running (name, `Created created |> Run_parameters.serialize)

let biokepi_machine =
  let host = Ketrew.EDSL.Host.parse "/tmp/KT-coclomachine/" in
  let max_processors = 7 in
  let run_program ?name ?(requirements = []) p =
    let open Ketrew.EDSL in
    let how =
      if
        (List.mem ~set:requirements `Quick_run
         || List.mem ~set:requirements `Internet_access)
        && allow_daemonize
      then `On_server_node
      else `Submit_to_coclobas
    in
    let with_umask prog = Program.(sh "umask 000" && sh "whoami" && sh "groups" && prog) in
    match how with
    | `On_server_node ->
      daemonize ~host ~using:`Python_daemon (with_umask p)
    | `Submit_to_coclobas ->
      run_program_with_coclo 
        Program.(
          (* sh "sudo mkdir -m 777 -p /cloco-kube/playground" && *)
          sh "echo User" && sh "whoami" &&
          sh "echo Host" && sh "hostname" &&
          sh "echo Machine" && sh "uname -a" &&
          p |> with_umask)
  in
  let open Biokepi.Setup.Download_reference_genomes in
  let toolkit =
    Biokepi.Setup.Tool_providers.default_toolkit ()
      ~host
      ~install_tools_path
      ~run_program
      ~gatk_jar_location
      ~mutect_jar_location
      ~netmhc_tool_locations in
  Biokepi.Machine.create name
    ~pyensembl_cache_dir
    ~max_processors
    ~get_reference_genome:(fun name ->
        Biokepi.Setup.Download_reference_genomes.get_reference_genome name
          ~toolkit
          ~host ~run_program
          ~destination_path:reference_genomes_path)
    ~host
    ~toolkit
    ~run_program
    ~work_dir:(work_dir // "work")
