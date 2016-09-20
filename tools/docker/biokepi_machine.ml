
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
  netmhccons=env_exn_tool_loc "NETMHCPAN_TARBALL_URL" "NetMHCcons";
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
      Coclobas_ketrew_backend.Plugin.run_program
        ~base_url:"http://127.0.0.1:8082"
        ~image
        ~volume_mounts
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
