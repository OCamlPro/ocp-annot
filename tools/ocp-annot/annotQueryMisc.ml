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

(************************************************************************)
(* [query_pos "FILE:POS"] returns the information at FILE:POS sorted by
   the size of the expression *)

let query_at_pos file_pos =
  let split =
    let split = OcpString.split file_pos ',' in
    match split with
    | _ :: _ -> split
    | [] -> OcpString.split file_pos ':'
  in
  match split with
    [ file; pos ] ->
      let pos = int_of_string pos in
      let annot_file = (Filename.chop_extension file) ^ ".annot" in
      let annot_file = AnnotParser.parse_file annot_file in
      let { annot_infos } = annot_file in

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
      annot_file, List.sort Pervasives.compare infos

  | [pos_file; pos_line; pos_linepos ] ->
    let pos_line = int_of_string pos_line in
    let pos_linepos = int_of_string pos_linepos in
    let annot_file = (Filename.chop_extension pos_file) ^ ".annot" in
    let annot_file = AnnotParser.parse_file annot_file in
    let { annot_infos; annot_basenames } = annot_file in

    let pos_file =
      let basename = Filename.basename pos_file in
      try
        StringMap.find basename !annot_basenames
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
    annot_file, List.sort Pervasives.compare infos

  | _ -> failwith "wrong argument"

(************************************************************************)
(* Find the .annot containg a longident definition                      *)

let split_lident path =
  let prefix, _ = OcpString.cut_at path '(' in
  match OcpString.split prefix '.' with
  | [] -> Printf.kprintf failwith "ocp-annot: split_lident %S" path
  | modname :: idents -> modname, idents

(* [find_by_path max_rec f path] From current directory, perform a
   search in the project for the .annot containing the module on top of
   [path].  If found, call [f annot_file idents] where [idents] is the
   [path] within the module.
   If not found, search the parent directory. Do that [max_rec] times.
*)

let find_by_path c max_rec f path =
  let modname, idents = split_lident path in
  let modname_annot = modname ^ ".annot" in
  let rec iter dir level =
    if level > max_rec then
      Printf.kprintf failwith
        "ocp-annot: file matching %S not found" modname
    else
      let files = AnnotMisc.readdir dir in
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
        if not (StringSet.mem file AnnotMisc.skip_dirs) &&
          AnnotMisc.is_directory filename then
          iter_sub (dir,level,files) [] filename
        else
          iter_files dir level files

  and iter_sub dlf stack dir =
    let files = AnnotMisc.readdir dir in
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
        if not (StringSet.mem file AnnotMisc.skip_dirs) &&
          AnnotMisc.is_directory filename then
          iter_sub dlf ((dir,files) :: stack)
            filename
        else
          iter_subfiles dlf stack dir files

  in
  iter "." 0

let iter_idents annot_file f =
  let annot_file = AnnotParser.parse_file annot_file in
  List.iter (fun (loc, infos) ->
    List.iter (function
    | Type _ -> ()
    | Ident ident ->
      let ident = AnnotParser.parse_ident annot_file ident in
      f loc ident
    ) infos
  ) annot_file.annot_infos
