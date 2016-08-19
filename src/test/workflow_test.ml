
open Nonstd

let () =
  let base_url = "http://127.0.0.1:8082" in
  let wf =
    let open Ketrew.EDSL in
    let node1 =
      workflow_node without_product
        ~name:"Coclobas test should succeed"
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            Coclobas.Kube_job.Specification.(
              make ~image:"ubuntu"
                ["ls"; "-la"]
            )
        )
    in
    let node2 =
      workflow_node without_product
        ~name:"Coclobas test should fail"
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            Coclobas.Kube_job.Specification.(
              make ~image:"ubuntu"
                ["exit"; "1"]
            )
        )
    in
    let node3 =
      workflow_node without_product
        ~name:"Coclobas test uses secret"
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url
            Coclobas.Kube_job.Specification.(
              let path = "/ketrewkube/hello-world" in
              let cool_file =
                File_contents_mount.fresh
                  ~path "Hello world!"
              in
              make ~image:"ubuntu"
                ~volume_mounts:[`Constant cool_file]
                ["bash"; "-c";
                 sprintf "ls -la %s ;\
                          cat %s "
                   (Filename.dirname path) path]
            )
        )
    in
    let node4 =
      workflow_node without_product
        ~name:"Coclobas test uses Program.t"
        ~make:(
          Coclobas_ketrew_backend.Plugin.run_program
            ~base_url
            ~image:"ubuntu"
            Program.(
              chain [
                shf "whoami";
                shf "hostname";
                shf "echo \"ketrew playground: $KETREW_PLAYGROUND\""
              ]
            )
        )
    in
    workflow_node without_product
      ~name:"Coclobas test workflow"
      ~edges:[
        depends_on node1;
        depends_on node2;
        depends_on node3;
        depends_on node4;
      ]
  in
  Ketrew.Client.submit_workflow wf
