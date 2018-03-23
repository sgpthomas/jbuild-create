open Core
open Commandline

let exit_with_msg ?channel:ch n msg =
  let ch = match ch with
    | Some x -> x
    | None -> stdout
  in
  fprintf ch "%s\n" msg;
  exit n

let check_template_dir dir =
  match Sys.file_exists dir with
  | `No | `Unknown -> false
  | `Yes -> true

let copy_template args =
  File.copy_dir ~args ~dest:(Filename.concat "." args.proj_name)

let main args =
  if not (check_template_dir (args.template_dir)) then
    exit_with_msg (-1) (sprintf "There is no template directory at: %s" args.template_dir);

  copy_template args;
  printf "Project Name: %s\n" args.proj_name

let () = Commandline.parse main
