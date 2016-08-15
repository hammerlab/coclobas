
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
    workflow_node without_product
      ~edges:[
        depends_on node1;
        depends_on node2;
      ]
  in
  Ketrew.Client.submit_workflow wf
