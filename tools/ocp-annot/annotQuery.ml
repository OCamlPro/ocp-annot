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

let skip_dirs =
  StringSet.of_list [
    ".git"; "_obuild"; "_build"; ".svn";
  ]

(*
KNOWN BUGS:
* Positions are currently given as absolute positions in files, which is
  known to be corrupted when dealing with generated files. Instead, we
  should use FILE:LINE:LINEPOS in the next version to avoid this problem.
*)

(************************************************************************)
(* Format: on the long term, we should support more than Emacs !        *)

type output =
| Int of int
| String of string
| Record of (string * output) list
| List of output list


let env_PATH =
  try OcpString.split (Sys.getenv "PATH") ':' with _ -> []

let has_tool tool =
  try Some (DirPath.find_in_path env_PATH tool) with _ -> None

module Emacs = struct

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

end

module Json = struct

  let string_of_output v =
    let b = Buffer.create 100 in
    let rec iter v =
      match v with
      | Int n -> Printf.bprintf b "%d" n
      | String s -> Printf.bprintf b "%S" s
      | Record labels ->
        Printf.bprintf b "{";
        List.iteri (fun i (label, v) ->
          if i > 0 then Buffer.add_char b ',';
          Printf.bprintf b "\"%s\": " label;
          iter v;
        ) labels;
        Printf.bprintf b "}";
      | List values ->
        Printf.bprintf b "[ ";
        List.iteri (fun i v ->
          if i > 0 then Buffer.add_string b ", ";
          iter v;
        ) values;
        Printf.bprintf b "]";
    in
    iter v;
    Buffer.contents b

end

let output_function = ref Emacs.string_of_output
let output_config = ref Emacs.output_config

(* The Emacs mode can read both absolute positions in file and
   line:linepos positions. We use the later because it better supports
   preprocessed files with # directives. *)

let use_absolute_pos = ref false (* no argument yet ! *)

let pos_of_pos pos =
  if !use_absolute_pos then
    Int pos.pos_pos
  else
    List [Int pos.pos_line; Int pos.pos_linepos ]

(************************************************************************)
(* [query_pos "FILE:POS"] returns the information at FILE:POS sorted by
   the size of the expression *)

let query_at_pos file_pos =
  match OcpString.split file_pos ':' with
    [ file; pos ] ->
      let pos = int_of_string pos in
      let annot_file = (Filename.chop_extension file) ^ ".annot" in
      let { annot_infos } = AnnotParser.parse_file annot_file in

      let rec iter infos locs =
        match locs with
        | [] -> infos
        | ( loc,new_infos) :: locs ->
          match loc with
          | Unbounded _
          | Uncomparable _
          | Unlocated -> iter infos locs
          | Bounded (loc1, loc2) ->
            if loc1.pos_pos <= pos && loc2.pos_pos >= pos then
              iter ( (loc2.pos_pos-loc1.pos_pos, loc, new_infos)
                     :: infos) locs
            else
              iter infos locs
      in
      let infos = iter [] annot_infos in
      List.sort Pervasives.compare infos

  | [pos_file; pos_line; pos_linepos ] ->
    let pos_line = int_of_string pos_line in
    let pos_linepos = int_of_string pos_linepos in
    let annot_file = (Filename.chop_extension pos_file) ^ ".annot" in
    let { annot_infos; annot_basenames } = AnnotParser.parse_file annot_file in

    let pos_file =
      let basename = Filename.basename pos_file in
      try
        StringMap.find basename annot_basenames
      with Not_found ->
        Printf.kprintf failwith "ocp-annot: annotations not for %s" pos_file
    in
    let pos = { pos_file; pos_line; pos_linepos; pos_pos = 0 } in

    let rec iter infos locs =
      match locs with
      | [] -> infos
      | (loc, new_infos) :: locs ->
        if AnnotParser.LOC.includes loc pos then
          iter ( (AnnotParser.LOC.approx loc,
                  loc, new_infos) :: infos) locs
        else
          iter infos locs
    in
    let infos = iter [] annot_infos in
    List.sort Pervasives.compare infos

  | _ -> failwith "wrong argument"

(************************************************************************)
(* Query information on the expression at a given position              *)

let query_info_file_pos file_pos =
  match query_at_pos file_pos with
  | [] -> failwith "ocp-annot: no info found at file pos"
  | infos ->
    let rec iter infos =
      match infos with
      | [] -> []
      | (_, loc, kinds) :: infos ->

        match infos with
          (_, loc2, kinds2) :: _
            when
(*
              (Printf.eprintf
                 "%S = %S && %s = %s ?\n%!"
                 (String.concat "," (List.map AnnotParser.KIND.to_string kinds))
                 (String.concat "," (List.map AnnotParser.KIND.to_string kinds2))
                 (match AnnotParser.LOC.end_pos loc with
                   None -> "-" | Some pos -> AnnotParser.POS.to_string (pos))
                 (match AnnotParser.LOC.end_pos loc2 with
                   None -> "-" | Some pos -> AnnotParser.POS.to_string (pos))
              ;
  true ) && *)
              kinds = kinds2 &&
              AnnotParser.LOC.end_pos loc =
              AnnotParser.LOC.end_pos loc2 ->
                iter infos
        | _ ->
          let (pos1, pos2) = match loc with
              Bounded (pos1, pos2) -> pos1, pos2
            | Unbounded pos1
            | Uncomparable (pos1, _) -> pos1, pos1
            | Unlocated -> assert false
          in
          ( "left", pos_of_pos pos1 ) ::
            ( "right", pos_of_pos pos2 ) ::
            List.map (function
            | Type typ ->
              "type", String (String.concat "\n" typ)
            | Ident ident ->
              "ident", Record (
                match AnnotParser.parse_ident ident with
                | Def (ident, loc) ->
                  [ "kind", String "def";
                    "ident", String ident ] @
                    (match loc with
                    | Bounded (loc1, loc2) ->
                      [ "scope_begin", pos_of_pos loc1;
                        "scope_end", pos_of_pos loc2 ]
                    | Unbounded loc ->
                      [ "scope_begin", pos_of_pos loc ]
                    | Unlocated
                    | Uncomparable _ -> []
                    )
                | ExtRef ident ->
                  [ "kind", String "ext_ref";
                    "ident", String ident ]
                | IntRef (ident, loc) ->
                  [ "kind", String "int_ref";
                    "ident", String ident;
                    "def_begin", pos_of_pos pos1;
                    "def_end", pos_of_pos pos2 ]
              )
            ) kinds
          @ [ "parent", Record (iter infos)]
    in
    let v = Record (iter infos) in
    Printf.printf "%s\n%!" (!output_function v)

(************************************************************************)
(* Find the .annot containg a longident definition                      *)

let is_directory file =
  (try Sys.is_directory file with _ -> false)

let find_by_path max_rec f path =
  let path, _ = OcpString.cut_at path '(' in
  let path = OcpString.split path '.' in
  match path with
  | [] -> assert false
  | modname :: idents ->
    let modname_annot = modname ^ ".annot" in
    let rec iter dir level =
      if level > max_rec then
        Printf.kprintf failwith
          "ocp-annot: file matching %S not found" modname
      else
        let files = Sys.readdir dir in
        let files = Array.to_list files in
        iter_files dir level files

    and iter_files dir level files =
      match files with
        [] -> iter (Filename.concat dir "..") (level+1)
      | file :: files ->
        let filename = Filename.concat dir file in
        if String.capitalize file = modname_annot then
          f filename idents
        else
          if not (StringSet.mem file skip_dirs) && is_directory filename then
            iter_sub (dir,level,files) [] filename
          else
            iter_files dir level files

    and iter_sub dlf stack dir =
      let files = Sys.readdir dir in
      let files = Array.to_list files in
      iter_subfiles dlf stack dir files

    and iter_subfiles dlf stack dir files =
      match files with
        [] ->
          begin
            match stack with
            | [] ->
              let (dir, level, files) = dlf in
              iter_files dir level files
            | (dir, files) :: stack ->
              iter_subfiles dlf stack dir files
          end
      | file :: files ->
        let filename = Filename.concat dir file in
        if String.capitalize file = modname_annot then
          f filename idents
        else
          if not (StringSet.mem file skip_dirs) && is_directory filename then
            iter_sub dlf ((dir,files) :: stack)
              filename
          else
            iter_subfiles dlf stack dir files

    in
    iter "." 0

(************************************************************************)
(* Query info to perform a jump to the identifier location              *)

let find_jump_for_ident annot_file idents =
  let { annot_infos } = AnnotParser.parse_file annot_file in
  let rec iter locs =
    match locs with
    | [] ->
      Printf.kprintf failwith
        "ocp-annot: cannot locate %S"
        (String.concat "." idents)
    | (loc, infos) :: locs ->
      iter_infos loc infos locs

  and iter_infos loc infos locs =
    match infos with
    | [] -> iter locs
    | Type _ :: infos -> iter_infos loc infos locs
    | Ident ident :: infos ->
      match AnnotParser.parse_ident ident with
      | IntRef _
      | ExtRef _ -> iter_infos loc infos locs
      | Def (ident, scope) ->
        match idents with
        | id :: _ when id = ident ->
          begin match loc with
          | Bounded (loc, _)
          | Unbounded loc
          | Uncomparable (loc, _) ->
            let ml_file =
              Filename.concat (Filename.dirname annot_file)
                loc.pos_file.file_basename in
            [
              "file", String ml_file;
              "pos", pos_of_pos loc;
            ]
          | Unlocated -> iter locs
          end
        | _ -> iter_infos loc infos locs
  in
  iter annot_infos

let query_jump_file_pos file_pos =
  match query_at_pos file_pos with
  | [] -> failwith "ocp-annot: no info found at pos"
  |  ( _, loc, infos ) :: _ ->
    let rec iter infos =
      match infos with
      | [] -> failwith "ocp-annot: info contains no reference to definition"
      | info :: infos ->
        match info with
        | Type _ -> iter infos
        | Ident ident ->
          match AnnotParser.parse_ident ident with
          | Def _ -> failwith "ocp-annot: info is a definition"
          | IntRef (ident, location) ->
            begin
              match location with
              | Bounded (loc, _)
              | Unbounded loc
              | Uncomparable (loc, _) ->
                [
                  "ident", String ident;
                  "pos", pos_of_pos loc;
                ]
              | Unlocated -> failwith "ocp-annot: info with no location"
            end
          | ExtRef path ->
            let src_file = AnnotParser.LOC.file loc in
            let file_path = OcpString.split src_file '/' in
            let file_path = match file_path with
              | ( "" | "." ) :: file_path -> file_path
              | _ -> file_path in
            let max_rec = min 3 (List.length file_path - 1) in
            ("ident", String path) ::
              find_by_path max_rec find_jump_for_ident path
    in
    let v = Record (iter infos) in
    Printf.printf "%s\n%!" (!output_function v)

let query_jump_long_ident path =
  let v = Record (
    ("ident", String path) ::
    find_by_path 0 find_jump_for_ident path
  ) in
  Printf.printf "%s\n%!" (!output_function v)

(************************************************************************)
(* Query alternate file (interface/implementation)                      *)

let query_alternate_file filename =
  let infos =
    if Filename.check_suffix filename ".ml"
      || Filename.check_suffix filename ".mll" then
      let mli_file = Filename.chop_extension filename ^ ".mli" in
      let should_create = not (Sys.file_exists mli_file) in
      [
        "file", String mli_file;
        "kind", String "interface";
      ] @
        (if should_create then ["create", String "t"]
         else [])
    else
    if Filename.check_suffix filename ".mli" then
      let basefile = Filename.chop_suffix filename ".mli" in
      let mll_file = basefile ^ ".mll" in
      if Sys.file_exists mll_file then
        [
        "file", String mll_file;
        "kind", String "lexer";
        ] else
        let ml_file = basefile ^ ".ml" in
        let should_create = not (Sys.file_exists ml_file) in
        [
          "file", String ml_file;
          "kind", String "implementation";
        ] @
          (if should_create then ["create", String "t"]
           else [])
    else
      [ "file", String filename;
        "kind", String "no-alternate" ]
  in
  Printf.printf "%s\n%!" (!output_function (Record infos))

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

(* Parse arguments and call actions                                     *)

let emacs_mode () =
  output_function := Emacs.string_of_output;
  output_config := Emacs.output_config

let json_mode () =
  output_function := Json.string_of_output

let output_config () = !output_config ()

let wrap f arg =
  try
    f arg
  with exn ->
    let bt = Printexc.get_backtrace () in
    let oc =
      open_out_gen [ Open_creat; Open_append ] 0o644 "/tmp/ocp-annot.log" in
    Printf.fprintf oc
      "'%s'\n" (String.concat "' '"
                  (Array.to_list Sys.argv));
    Printf.fprintf oc
      "Error: exception %s\n" (Printexc.to_string exn);
    if bt <> "" then
      Printf.fprintf oc "Backtrace:\n%s\n%!" bt;
    close_out oc;
    let infos = [ "error",
                  String (Printf.sprintf "Error: exception %s"
                            (Printexc.to_string exn)) ] in
    let v = Record infos in
    Printf.printf "%s\n%!" (!output_function v)
