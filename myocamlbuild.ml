open Printf
open Solvuu_build.Std

let project_name = "coclobas"
let version = "master"

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

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize project_name);
]

let () =
  Project.basic1 ~project_name ~version [lib;app]
    ~ocamlinit_postfix
