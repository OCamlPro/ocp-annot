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

let env_PATH =
  try OcpString.split (Sys.getenv "PATH") ':' with _ -> []

let has_tool tool =
  try Some (DirPath.find_in_path env_PATH tool) with _ -> None

let string_of_output v =
  let b = Buffer.create 100 in
  let rec iter v =
    match v with
    | Int n -> Printf.bprintf b "%d" n
    | String s -> Printf.bprintf b "%S" s
    | Record labels ->
      Printf.bprintf b "(";
      List.iter (fun (label, v) ->
        Printf.bprintf b "(:%s . " label;
        iter v;
        Printf.bprintf b ")"
      ) labels;
      Printf.bprintf b ")";
    | List values ->
      Printf.bprintf b "(";
      List.iter (fun v ->
        Printf.bprintf b " ";
        iter v;
      ) values;
      Printf.bprintf b ")";
  in
  iter v;
  Buffer.contents b

let output_config () =
  Printf.printf "%s" (List.assoc "files/ocp-annot.el" AnnotFiles.files);
  Printf.printf "(defun ocp-annot-when-no-merlin() %s)\n"
    (match has_tool "merlin" with
    | Some _ -> "()"
    | None -> "(ocp-annot-mode)");
  Printf.printf "(defun ocp-annot-when-no-ocamlspot() %s)\n"
    (match has_tool "ocamlspot" with
    | Some _ -> "()"
    | None -> "(ocp-annot-mode)");
  Printf.printf "%!"
