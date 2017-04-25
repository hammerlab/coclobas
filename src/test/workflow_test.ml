
open Nonstd

type params = {
  port: int [@env "PORT"];
  test_kind: [ `Local_docker | `Kubernetes | `Aws_batch ]
      [@enum [ "local-docker", `Local_docker;
               "kubernetes", `Kubernetes;
               "aws-batch", `Aws_batch ]];
} [@@deriving cmdliner]

let main {port; test_kind} =
  let base_url = sprintf "http://127.0.0.1:%d" port in
  let tags more = "coclobas" :: "test" :: more in
  let open Ketrew.EDSL in
  let of_command expect_tag cmd =
    workflow_node without_product
      ~name:(sprintf "Coclobas test should %s" expect_tag)
      ~tags:(tags ["should-" ^ expect_tag])
      ~make:(
        match test_kind with
        | `Aws_batch ->
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Aws_batch_job.Specification.make ~image:"ubuntu" cmd
             |> Coclobas.Job.Specification.aws_batch)
        | `Kubernetes ->
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Kube_job.Specification.make ~image:"ubuntu" cmd
             |> Coclobas.Job.Specification.kubernetes)
        | `Local_docker ->
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Local_docker_job.Specification.make ~image:"ubuntu" cmd
             |> Coclobas.Job.Specification.local_docker)
      )
  in
  let of_program tag p =
    let prog_string = Ketrew_pure.Program.to_single_shell_command p in
    workflow_node without_product
      ~name:(sprintf "Coclobas test uses Program.t (%d bytes)"
               (String.length prog_string))
      ~tags:(tags ["should-" ^ tag; "program"])
      ~make:(
        match test_kind with
        | `Aws_batch ->
          Coclobas_ketrew_backend.Plugin.aws_batch_program
            ~base_url
            ~image:"ocaml/opam" p
        | `Kubernetes ->
          Coclobas_ketrew_backend.Plugin.kubernetes_program
            ~base_url
            ~image:"ubuntu" p
        | `Local_docker ->
          Coclobas_ketrew_backend.Plugin.local_docker_program
            ~base_url
            ~image:"ubuntu" p
      )
  in
  let wf =
    let node1 = of_command "succeed" ["ls"; "-la"] in
    let node2 = of_command "fail" ["exit"; "1"] in
    let node3 =
      workflow_node without_product
        ~name:"Coclobas test uses secret"
        ~tags:(tags ["succeeds"; "secret"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Job.Specification.kubernetes
               Coclobas.Kube_job.Specification.(
                 let path = "/ketrewkube/hello-world" in
                 let cool_file =
                   File_contents_mount.make ~path "Hello world!" in
                 make ~image:"ubuntu"
                   ~volume_mounts:[`Constant cool_file]
                   ["bash"; "-c";
                    sprintf "ls -la %s ;\
                             cat %s "
                      (Filename.dirname path) path]
               ))
        )
    in
    let node4 = of_program "succeed" Program.(
        chain [
          shf "whoami";
          shf "hostname";
          shf "echo \"ketrew playground: $KETREW_PLAYGROUND\"";
          shf "sleep 60";
        ]
      ) in
    let node4big = of_program "succeed" Program.(
        chain (
          List.init 100 ~f:(fun i -> shf "echo 'This is the command %d'" i)
        )
      ) in
    let node5 =
      workflow_node without_product
        ~name:"Coclobas test local-docker"
        ~tags:(tags ["succeeds"; "local-docker"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Job.Specification.local_docker
               Coclobas.Local_docker_job.Specification.(
                 make ~image:"ubuntu"
                   ~volume_mounts:[`Local ("/usr/bin", "/hostusrbin")]
                   ["bash"; "-c"; sprintf "echo sleepin ; sleep 120 ; ls -la /hostusrbin"]
               ))
        )
    in
    let node6 =
      workflow_node without_product
        ~name:"Coclobas local-docker wrong image"
        ~tags:(tags ["fails"; "local-docker"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Job.Specification.local_docker
               Coclobas.Local_docker_job.Specification.(
                 make ~image:"ubuntuijdeidejiije/djedidjede"
                   ~volume_mounts:[`Local ("/usr/bin", "/hostusrbin")]
                   ["bash"; "-c"; sprintf "ls -la /hostusrbin"]
               ))
        )
    in
    let node7 =
      workflow_node without_product
        ~name:"Coclobas local-docker with program"
        ~tags:(tags ["succeeds"; "local-docker"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.local_docker_program
            ~base_url ~image:"ubuntu"
            ~volume_mounts:[`Local ("/usr/bin", "/hostusrbin")]
            Program.(
              exec ["find"; "/hostusrbin"]
            )
        )
    in
    let kubes = [
      depends_on node1;
      depends_on node2;
      depends_on node3;
      depends_on node4;
      depends_on node4big;
    ] in
    let locals = [
      depends_on node1;
      depends_on node2;
      depends_on node4;
      depends_on node4big;
      depends_on node5;
      depends_on node6;
      depends_on node7;
    ] in
    let aws = [
      depends_on node1;
      depends_on node2;
      depends_on node4;
      depends_on node4big;
    ] in
    let edges =
      match test_kind with
      | `Kubernetes -> kubes
      | `Local_docker -> locals
      | `Aws_batch -> aws in
    workflow_node without_product
      ~tags:(tags ["fails"; "toplevel"])
      ~name:"Coclobas test workflow"
      ~edges
  in
  Ketrew.Client.submit_workflow wf

let () =
  match
    Cmdliner.Term.(eval
                     (pure main $ params_cmdliner_term (), info "test-workflow"))
  with
  | `Error _ -> exit 1
  | `Ok ()
  | `Version
  | `Help -> exit 0