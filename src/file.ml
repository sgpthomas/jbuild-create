open Core
open Re2
open Commandline

let is_directory ?follow_symlinks:(syms=true) file =
  match Sys.is_directory file with
  | `No | `Unknown -> false
  | `Yes -> true

let read_dir ?r:(r=false) dir =
  let rec read base dir =
    let all = (Sys.ls_dir dir) in
    let all = List.map all ~f:(fun x -> Filename.concat dir x) in
    let dirs = List.filter all ~f:(is_directory) in
    let files = List.filter all ~f:(fun x -> not (is_directory x)) in
    if r then
      (List.join @@ List.map dirs ~f:(fun x -> read (sprintf "%s/" x) x)) @ files
    else
      files
  in
  (read "" dir)

let replace ~args mtch =
  match Regex.Match.get ~sub:(`Index 0) mtch with
  | Some "@@PROJNAME@@" -> args.proj_name
  | Some x -> x
  | None -> ""

let reform_file ~prefix ~file ~dest =
  let prefix = Filename.parts prefix in
  let filep = Filename.parts file in
  let key = List.mapi filep ~f:(fun i x -> if List.nth prefix i = Some x then true else false)
  in
  let reconstruct l =
    List.fold l ~init:dest ~f:(fun accum x -> Filename.concat accum x)
  in
  match List.zip key filep with
  | Some x -> reconstruct @@
    List.map (List.drop_while x ~f:(fun (b, _) -> b)) ~f:(fun (_, x) -> x)
  | None -> "" (* Probably should fail here *)

let rename_template_file ~args ~file =
  let (dir, base) = Filename.split file in
  let f root str =
    if String.equal root "template" then
      Filename.concat dir str
    else
      file
  in
  match Filename.split_extension base with
    | s, Some ext -> f s @@ sprintf "%s.%s" args.proj_name ext
    | s, None -> f s @@ sprintf "%s" s

let copy_file ~args ~file ~src ~dest =
  let cont = In_channel.read_all file in
  let r = match Regex.create "@@([[:alpha:]])*@@" with
    | Ok x -> x
    | Error _ -> failwith "Failed to compile regex"
  in

  let cont = match Regex.replace ~f:(replace ~args) r cont with
    | Ok x -> x
    | Error _ -> cont
  in
  let file = rename_template_file ~args ~file in
  printf "%s\n" file;
  let outf = reform_file ~prefix:src ~file ~dest in
  let (dir, _) = Filename.split outf in
  Unix.mkdir_p dir;
  let fd = Unix.openfile ~mode:[Unix.O_WRONLY; Unix.O_CREAT] outf in
  let _nwrit = UnixLabels.write_substring fd ~buf:cont ~pos:0 ~len:(String.length cont) in
  Unix.close fd

let copy_dir ~args ~dest =
  let src = args.template_dir in
  let files = read_dir ~r:true src in
  List.iter files ~f:(fun x -> copy_file ~args ~file:x ~src ~dest)
