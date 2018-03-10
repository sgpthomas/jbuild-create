open Core
open Commandline

let template_dir = "~/.local/share/jbuild-create/templates"

let exit_with_msg ?channel:ch n msg =
  let ch = match ch with
    | Some x -> x
    | None -> stdout
  in
  fprintf ch "%s\n" msg;
  exit n

let check_template_dir () =
  match Sys.file_exists template_dir with
  | `No | `Unknown -> false
  | `Yes -> true

let main args =
  if not (check_template_dir ()) then
    exit_with_msg (-1) (sprintf "There is no template directory at: %s" template_dir);
  printf "%s\n" args.proj_name

let () = Commandline.parse main
