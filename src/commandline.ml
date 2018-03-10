open Core
open Cmdliner

type project = Library | Binary
type options = {
  proj_name: string;
  proj_type: project
}

let parse (main: options -> unit) =

  let project =
    let doc = "Generate a library template" in
    let lib = Library, Arg.info ["l"; "lib"] ~doc in
    let doc = "Generate a binary template" in
    let bin = Library, Arg.info ["b"; "bin"] ~doc in
    Arg.(last & vflag_all [Binary] [lib; bin])
  in

  let proj_name =
    let doc = "Generate a binary template" in
    Arg.(value & pos 0 string "" & info [] ~doc ~docv:"NAME")
  in

  let start proj name =
    let args = {
      proj_name = name;
      proj_type = proj
    }
    in
    main args
  in

  let info =
    let doc = "OCaml project generator" in
    Term.info "jbuild-create" ~version:"v0.1" ~doc ~exits:Term.default_exits
  in

  let chorus_t = Term.(const start $ project $ proj_name) in
  Term.exit @@ Term.eval (chorus_t, info)
