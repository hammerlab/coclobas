
open Nonstd

let which_ones =
  match Sys.getenv "kind" with
  | "local-docker" -> `Local_docker
  | "kubernetes" -> `Kubernetes
  | other -> `All
  | exception _ -> `All

let () =
  let base_url = "http://127.0.0.1:8082" in
  let tags more = "coclobas" :: "test" :: more in
  let wf =
    let open Ketrew.EDSL in
    let node1 =
      workflow_node without_product
        ~name:"Coclobas test should succeed"
        ~tags:(tags ["succeeds"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Kube_job.Specification.make ~image:"ubuntu" ["ls"; "-la"]
             |> Coclobas.Job.Specification.kubernetes)
        )
    in
    let node2 =
      workflow_node without_product
        ~name:"Coclobas test should fail"
        ~tags:(tags ["fails"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            (Coclobas.Kube_job.Specification.make ~image:"ubuntu" ["exit"; "1"]
             |> Coclobas.Job.Specification.kubernetes)
        )
    in
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
    let node4 =
      workflow_node without_product
        ~name:"Coclobas test uses Program.t"
        ~tags:(tags ["succeeds"; "program"])
        ~make:(
          Coclobas_ketrew_backend.Plugin.kubernetes_program
            ~base_url
            ~image:"ubuntu"
            Program.(
              chain [
                shf "whoami";
                shf "hostname";
                shf "echo \"ketrew playground: $KETREW_PLAYGROUND\"";
                shf "sleep 60";
              ]
            )
        )
    in
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
    ] in
    let locals = [
        depends_on node5;
        depends_on node6;
        depends_on node7;
      ] in
    let edges =
      match which_ones with
      | `Kubernetes -> kubes
      | `Local_docker -> locals
      | `All -> kubes @ locals in
    workflow_node without_product
      ~tags:(tags ["fails"; "toplevel"])
      ~name:"Coclobas test workflow"
      ~edges
  in
  Ketrew.Client.submit_workflow wf
