open Core
open Cmdliner

type project = Library | Binary
type options = {
  proj_name: string;
  proj_type: project;
  template_dir: string;
}

let default_template_dir = "~/.local/share/jbuild-create/templates"

let expand_home file =
  if String.equal (String.prefix file 1) "~" then
    (let home = match Sys.getenv "HOME" with
        | Some x -> x
        | None -> failwith "You must run this as a user."
     in
     Filename.concat home (String.drop_prefix file 1))
  else
    file

let filename_of_proj_type = function
  | Library -> "library"
  | Binary -> "binary"

let parse (main: options -> unit) =
  let project =
    let doc = "Generate a library template" in
    let lib = Library, Arg.info ["l"; "lib"] ~doc in
    let doc = "Generate a binary template" in
    let bin = Binary, Arg.info ["b"; "bin"] ~doc in
    Arg.(last & vflag_all [Binary] [lib; bin])
  in

  let proj_name =
    let doc = "Generate a binary template" in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc ~docv:"NAME")
  in

  let template_dir =
    let doc = "Specify the template directory" in
    Arg.(value & opt string default_template_dir & info ["template"] ~doc )
  in

  let start proj name dir =
    let args = {
      proj_name = name;
      proj_type = proj;
      template_dir = Filename.concat (expand_home dir) (filename_of_proj_type proj);
    }
    in
    main args
  in

  let info =
    let doc = "OCaml project generator" in
    Term.info "jbuild-create" ~version:"v0.1" ~doc ~exits:Term.default_exits
  in

  let chorus_t = Term.(const start $ project $ proj_name $ template_dir) in
  Term.exit @@ Term.eval (chorus_t, info)
