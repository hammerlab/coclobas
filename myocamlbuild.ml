open Printf
open Solvuu_build.Std

let project_name = "coclobas"
let version = "master"

let build_tests =
  try Sys.getenv "WITH_TESTS" = "true" with _ -> false

let findlib_deps = [
  "nonstd";
  "sosa";
  "pvem_lwt_unix";
  "cohttp.lwt";
  "irmin.git";
  "irmin.unix";
  "cmdliner";
  "ppx_deriving.std";
  "ppx_deriving_yojson";
  "uuidm";
]

let lib : Project.item =
  Project.lib project_name
    ~thread:()
    ~findlib_deps
    ~dir:"src/lib"
    ~pack_name:project_name
    ~pkg:project_name

let app : Project.item =
  Project.app project_name
    ~thread:()
    ~file:"src/app/main.ml"
    ~internal_deps:[lib]

let test : Project.item =
  Project.app (project_name ^ "-test")
    ~thread:()
    ~file:"src/test/client_server.ml"
    ~internal_deps:[lib]

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize project_name);
]

let () =
  Project.basic1 ~project_name ~version ~ocamlinit_postfix
    (if build_tests then [lib; app; test] else [lib; app])
