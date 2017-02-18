(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open StringCompat
open AnnotParser.TYPES
open AnnotQueryTypes


let use_absolute_pos = ref false (* no argument yet ! *)

let output_function = ref (fun _ -> assert false)
let output_config = ref (fun () -> assert false)



let env_PATH = lazy
  (try OcpString.split (Sys.getenv "PATH") ':' with _ -> [])


let is_directory file =
  try Sys.is_directory file with _ -> false
let readdir dir =
  try Sys.readdir dir with _ -> [||]

let rec iter_files dir f =
  let files = readdir dir in
  Array.iter (fun file ->
    let filename = Filename.concat dir file in
    if is_directory filename then
      iter_files filename f
    else
      f filename
  ) files

let has_tool tool =
  try Some (DirPath.find_in_path (Lazy.force env_PATH) tool) with _ -> None

(* The Emacs mode can read both absolute positions in file and
   line:linepos positions. We use the later because it better supports
   preprocessed files with # directives. *)

let pos_of_pos pos =
  if !use_absolute_pos then
    Int pos.pos_pos
  else
    List [Int pos.pos_line; Int pos.pos_linepos ]


let is_project_root dir =
  let files = try Sys.readdir dir with _ -> [||] in
  try
    Array.iter (fun file ->
      match file with
      | "_obuild" | "_build" | ".git" -> raise Exit
      | _ -> ()
    ) files;
    false
  with Exit -> true

(* returns None if not found, Some (dir, max_rec) otherwise *)
let find_project_root () =
  let rec iter dir max_rec =
    if is_project_root dir then
      Some (dir, max_rec) else
      let dirdir = Filename.dirname dir in
      if dirdir = dir then
        None
      else
        iter dirdir (max_rec+1)
  in
  iter (Sys.getcwd ()) 0

let find_max_rec c src_file =
  match find_project_root () with
  | Some (_, max_rec) -> max_rec
  | None ->
        (* use another heuristic *)
    let file_path = OcpString.split src_file '/' in
    let file_path = match file_path with
      | ( "" | "." ) :: file_path -> file_path
      | _ -> file_path in
    min c.max_rec (List.length file_path - 1)

let start_time = Unix.gettimeofday ()
let check_time c =
  let time = Unix.gettimeofday () in
  if time -. start_time > c.timeout then failwith "ocp-annot: timeout"

let with_log f =
  let pid = Unix.getpid () in
  let oc =
    open_out_gen [ Open_creat; Open_append ] 0o644 "/tmp/ocp-annot.log" in
  let printer s = Printf.fprintf oc "%d: %s\n" pid s in
  f printer;
  close_out oc

let log_exn exn  =
  let bt = Printexc.get_backtrace () in
  with_log (fun printer ->
    Printf.kprintf printer "dir: %S" (Sys.getcwd ());
    Printf.kprintf printer
      "'%s'" (String.concat "' '"
                  (Array.to_list Sys.argv));
    Printf.kprintf printer
      "Error: exception %s" (Printexc.to_string exn);
    if bt <> "" then
      Printf.kprintf printer "Backtrace:\n%s\n%!" bt
  )
