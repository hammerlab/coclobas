
open Nonstd

let () =
  let wf =
    let open Ketrew.EDSL in
    let node1 =
      workflow_node without_product
        ~name:"coclobas test should succeed"
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url:"http://localhost:8082"
            Coclobas.Kube_job.Specification.(
              fresh ~image:"ubuntu"
                ["ls"; "-la"]
            )
        )
    in
    let node2 =
      workflow_node without_product
        ~name:"coclobas test should fail"
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url:"http://localhost:8082"
            Coclobas.Kube_job.Specification.(
              fresh ~image:"ubuntu"
                ["exit"; "1"]
            )
        )
    in
    let node3 =
      workflow_node without_product
        ~name:"coclobas test uses secret"
        ~make:(
          Coclobas_ketrew_backend.Plugin.create
            ~base_url:"http://localhost:8082"
            Coclobas.Kube_job.Specification.(
              let path = "/ketrewkube/hello-world" in
              let cool_file =
                File_contents_mount.fresh
                  ~path "Hello world!"
              in
              fresh ~image:"ubuntu"
                ~volume_mounts:[`Constant cool_file]
                ["bash"; "-c";
                 sprintf "ls -la %s ;\
                          cat %s "
                   (Filename.dirname path) path]
            )
        )
    in
    workflow_node without_product
      ~edges:[
        depends_on node1;
        depends_on node2;
        depends_on node3;
      ]
  in
  Ketrew.Client.submit_workflow wf
