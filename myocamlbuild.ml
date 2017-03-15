open Nonstd
open Solvuu_build.Std

let project_name = "coclobas"
let version = "0.0.1"

let build_tests =
  try Sys.getenv "WITH_TESTS" = "true" with _ -> false

let findlib_deps = [
  "nonstd";
  "sosa";
  "pvem_lwt_unix";
  "cohttp.lwt";
  "trakeva_of_uri";
  "cmdliner";
  "ppx_deriving.std";
  "ppx_deriving_yojson";
  "uuidm";
  "base64";
  "odate";
]

let lib : Project.item =
  Project.lib project_name
    ~thread:()
    ~findlib_deps
    ~dir:"src/lib"
    ~style:(`Pack project_name)

let ketrew_backend : Project.item option =
  let item =
    Project.lib (project_name ^ "_ketrew_backend")
      ~thread:()
      ~findlib_deps:("ketrew" :: findlib_deps)
      ~dir:"src/ketrew_backend"
      ~style:(`Pack (project_name ^ "_ketrew_backend"))
      ~internal_deps:[lib]
      ~install:(`Findlib (project_name ^ ".ketrew_backend"))
  in
  if Project.dep_opts_sat item ["ketrew"]
  then Some item
  else None

let app : Project.item =
  Project.app project_name
    ~thread:()
    ~file:"src/app/main.ml"
    ~internal_deps:[lib]

let test : Project.item option =
  if build_tests
  then Some (
      Project.app (project_name ^ "-test")
        ~thread:()
        ~file:"src/test/client_server.ml"
        ~install:`No
        ~internal_deps:[lib]
    ) else None

let linked_ketrew : Project.item option =
  match ketrew_backend with
  | Some kb ->
    Some (
      Project.app (project_name ^ "-ketrew")
        ~thread:()
        ~file:"src/test/cocloketrew.ml"
        ~internal_deps:[lib; kb]
    )
  | _ -> None

let test_ketrew_workflow : Project.item option =
  match build_tests, ketrew_backend with
  | true, Some kb ->
    Some (
      Project.app (project_name ^ "-ketrew-workflow-test")
        ~thread:()
        ~install:`No
        ~file:"src/test/workflow_test.ml"
        ~internal_deps:[lib; kb]
    )
  | _, _ -> None

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize_ascii project_name);
]

let () =
  Project.basic1 ~project_name ~version ~ocamlinit_postfix
    (List.filter_opt [
        Some lib;
        Some app;
        test;
        ketrew_backend;
        linked_ketrew;
        test_ketrew_workflow;
      ])
