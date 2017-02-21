(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open StringCompat

module TYPES = struct

  type file = {
    file_name : string;
    file_basename : string; (* we over-approximate that two files are equal
                               if they have the same basename, and are
                               included in the same file... *)
  }

  type position = {
    pos_file: file;
    pos_line: int;
    pos_linepos:  int; (* pos between char and beginning of line *)
    pos_pos : int; (* not used in comparison in POS, only for backward compat *)
  }

  type location =
  | Bounded of position * position
  (* (pos1, pos2) with LINEPOS.lte pos1 pos2 if LINEPOS.comparable pos1 pos2 *)
  | Unbounded of position
  | Uncomparable of position * position
  | Unlocated

  type kind =
    Type of string list
  | Ident of string

  type ident =
    Def of string * location (* scope *)
  | ExtRef of string
  | IntRef of string * location (* definition *)

  type annot_file = {
    annot_filename : string;
    annot_basenames : file StringMap.t ref;
    annot_infos : (location * kind list) list;
  }

end

open TYPES


module KIND = struct

  let to_string kind =
    match kind with
    | Type strings ->
      Printf.sprintf "Type \"%s\"" (String.concat "\" \"" strings)
    | Ident s ->
      Printf.sprintf "Ident %S" s

end

module POS = struct

  let to_string pos =
    Printf.sprintf "%s:%d:%d"
      pos.pos_file.file_name pos.pos_line pos.pos_linepos

  let comparable pos1 pos2 =
    pos1.pos_file.file_basename == pos2.pos_file.file_basename
  let gte pos1 pos2 =
    pos1.pos_line > pos2.pos_line ||
      (pos1.pos_line = pos2.pos_line &&
          pos1.pos_linepos >= pos2.pos_linepos)
  let gt pos1 pos2 =
    pos1.pos_line > pos2.pos_line ||
      (pos1.pos_line = pos2.pos_line &&
          pos1.pos_linepos > pos2.pos_linepos)
  let lt pos1 pos2 =
    pos1.pos_line < pos2.pos_line ||
      (pos1.pos_line = pos2.pos_line &&
          pos1.pos_linepos < pos2.pos_linepos)
  let lte pos1 pos2 =
    pos1.pos_line < pos2.pos_line ||
      (pos1.pos_line = pos2.pos_line &&
          pos1.pos_linepos <= pos2.pos_linepos)
  let approx pos = pos.pos_line * 1000 + pos.pos_linepos
end

module LOC = struct

  let to_string = function
    | Unlocated -> "Unlocated"
    | Uncomparable (pos1, pos2) ->
      Printf.sprintf "Uncomparable(%s,%s)"
        (POS.to_string pos1) (POS.to_string pos2)
    | Bounded (pos1, pos2) ->
      Printf.sprintf "Bounded(%s,%s)"
        (POS.to_string pos1) (POS.to_string pos2)
    | Unbounded pos1 ->
      Printf.sprintf "Unbounded(%s)" (POS.to_string pos1)

  let approx loc = match loc with
    | Unbounded pos -> max_int - POS.approx pos
    | Bounded (pos1, pos2) -> POS.approx pos2 - POS.approx pos1
    | Uncomparable _
    | Unlocated -> max_int

  let includes loc pos =
    match loc with
    | Bounded (pos1, pos2) ->
      POS.comparable pos pos1 &&
        POS.gte pos pos1 && POS.gte pos2 pos
    | Unbounded pos1 ->
      POS.comparable pos pos1 && POS.gte pos pos1
    | Uncomparable _
    | Unlocated -> false

  let includes loc pos =
    let includes = includes loc pos in
    (*
    Printf.eprintf "LOCATION.includes( %s , %s ) = %b\n%!"
      (to_string loc) (POS.to_string pos) includes;
    *)
    includes

  let file loc =
    match loc with
    | Bounded (pos,_)
    | Unbounded pos
    | Uncomparable (pos, _) -> pos.pos_file.file_name
    | Unlocated -> assert false

  let begin_pos loc =
    match loc with
      Bounded (pos, _)
    | Unbounded pos
    | Uncomparable (pos, _) -> Some pos
    | Unlocated -> None

  let end_pos loc =
    match loc with
      Bounded (_, pos)
    | Uncomparable (_, pos) -> Some pos
    | Unbounded _
    | Unlocated -> None

end


let error fn s =
  Printf.eprintf "Error: unexpected argument %S to function %s\n%!"
    s fn;
  exit 2

let int_of_string s =
  try
    int_of_string s
  with _ -> error "int_of_string" s

let get_file fileset file_name =
  let file_basename = Filename.basename file_name in
  try
    StringMap.find file_basename !fileset
  with Not_found ->
    let file = { file_basename; file_name } in
    fileset := StringMap.add file_basename file !fileset;
    file

let parse_pos fileset pos_file pos =
  match OcpString.split pos ' ' with
    "" :: line :: posline :: poschar :: ( [] | [ "" ]) ->
      let pos_line = int_of_string line in
      let posline = int_of_string posline in
      let pos_pos = int_of_string poschar in
      if pos_pos < 0 then None
      else
        let pos_linepos = pos_pos - posline in
        let pos_file = get_file fileset pos_file in
        Some { pos_file; pos_line; pos_linepos; pos_pos }
  | _ -> error "parse_pos" pos

let make_location pos1 pos2 =
  match pos1, pos2 with
  | None, _ -> Unlocated
  | Some pos, None -> Unbounded pos
  | Some pos1, Some pos2 ->
    if POS.comparable pos1 pos2 then
      Bounded (pos1, pos2)
    else
      Uncomparable (pos1, pos2)

let parse_location fileset line =
  (*  Printf.eprintf "parse_location %S\n%!" line; *)
  match OcpString.split line '"' with
    [ ""; file1; pos1; file2; pos2 ] ->
      let loc1 = parse_pos fileset file1 pos1 in
      let loc2 = parse_pos fileset file2 pos2 in
      make_location loc1 loc2
  | _ -> error "parse_location" line


let parse_ident fileset line =
  match OcpString.split line '"' with
    [ reference; file1; pos1; file2; pos2 ] ->
      begin match OcpString.split reference ' ' with
        [ "";""; kind; ident;"" ] ->
          let loc1 = parse_pos fileset file1 pos1 in
          let loc2 = parse_pos fileset file2 pos2 in
          let location = make_location loc1 loc2 in
          begin
            match kind with
            | "def" -> Def (ident, location)
            | "int_ref" -> IntRef (ident, location)
            | _ -> error "parse_reference.kind" kind;
          end
      | _ -> error "parse_reference.reference" reference
      end
  | [ reference ] ->
      begin match OcpString.split reference ' ' with
        [ "";""; "ext_ref"; ident ] ->  ExtRef ident
      | [ "";""; kind; pre; infix; ")"] -> ExtRef (pre ^ infix ^ ")")
      | _ -> error "parse_reference.simple_reference" reference
      end
  | _ -> error "parse_reference" line


let parse_file annot_filename =
  let fileset = ref StringMap.empty in
  let lines = FileLines.read_file annot_filename in
  let rec iter lines locations =
    match lines with
      [] | [ "" ] -> locations
    | line :: lines ->
      let location = parse_location fileset line in
      iter2 lines locations location []

  and iter2 lines locations location infos =
    match lines with
    | [] -> locations
    | "type(" :: lines ->
      iter3 lines locations location infos []
    | "ident(" :: reference :: ")" :: lines ->
      (*      let _ = parse_reference reference in *)
      iter2 lines locations location (Ident reference :: infos)
    | line :: lines ->
      let locations = (location, infos) :: locations in
      let location = parse_location fileset line in
      iter2 lines locations location []


  and iter3 lines locations location infos prev_lines =
    match lines with
    | [] -> assert false
    | ")" :: lines ->
      iter2 lines locations location ( (Type (List.rev prev_lines)) :: infos)
    | line :: lines ->
      iter3 lines locations location infos (line :: prev_lines)
  in
  let annot_infos = iter lines [] in
  let annot_basenames = fileset in
  { annot_filename; annot_infos; annot_basenames }

let parse_ident annot_file ident =
  parse_ident annot_file.annot_basenames ident
