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

module TYPES = struct

  type config = AnnotQueryTypes.config = {
    max_rec : int;
    query_chdir : string option;
    timeout : float;
  }

end

open TYPES

(************************************************************************)
(* Query information on the expression at a given position              *)

let query_info_file_pos c file_pos =
  match AnnotQueryMisc.query_at_pos file_pos with
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
          ( "left", AnnotMisc.pos_of_pos pos1 ) ::
            ( "right", AnnotMisc.pos_of_pos pos2 ) ::
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
                      [ "scope_begin", AnnotMisc.pos_of_pos loc1;
                        "scope_end", AnnotMisc.pos_of_pos loc2 ]
                    | Unbounded loc ->
                      [ "scope_begin", AnnotMisc.pos_of_pos loc ]
                    | Unlocated
                    | Uncomparable _ -> []
                    )
                | ExtRef ident ->
                  [ "kind", String "ext_ref";
                    "ident", String ident ]
                | IntRef (ident, loc) ->
                  [ "kind", String "int_ref";
                    "ident", String ident;
                    "def_begin", AnnotMisc.pos_of_pos pos1;
                    "def_end", AnnotMisc.pos_of_pos pos2 ]
              )
            ) kinds
          @ [ "parent", Record (iter infos)]
    in
    let v = Record (iter infos) in
    Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

(************************************************************************)
(* Query info to perform a jump to the identifier location              *)

let find_jump_for_ident annot_file idents =
  let { annot_infos } = AnnotParser.parse_file annot_file in
  let rec iter locs =
    match locs with
    | [] ->
      Printf.kprintf failwith "ocp-annot: cannot locate %S"
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
              "pos", AnnotMisc.pos_of_pos loc;
            ]
          | Unlocated -> iter locs
          end
        | _ -> iter_infos loc infos locs
  in
  iter annot_infos

let query_jump_file_pos c file_pos =
  match AnnotQueryMisc.query_at_pos file_pos with
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
                  "pos", AnnotMisc.pos_of_pos loc;
                ]
              | Unlocated -> failwith "ocp-annot: info with no location"
            end
          | ExtRef path ->
            let src_file = AnnotParser.LOC.file loc in
            let max_rec = AnnotMisc.find_max_rec c src_file in
            ("ident", String path) ::
              AnnotQueryMisc.find_by_path c max_rec find_jump_for_ident path
    in
    let v = Record (iter infos) in
    Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

let query_jump_long_ident c path =
  let v = Record (
    ("ident", String path) ::
      AnnotQueryMisc.find_by_path c
      (AnnotMisc.find_max_rec c ".") find_jump_for_ident path
  ) in
  Printf.printf "%s\n%!" (!AnnotMisc.output_function v)


(************************************************************************)
(* Query alternate file (interface/implementation)                      *)

let find_file_for_ident annot_file idents =
  let file = Filename.chop_extension annot_file in
  let mly_file = file ^ ".ml" in
  if Sys.file_exists mly_file then
    mly_file
  else
    let mll_file = file ^ ".mll" in
  if Sys.file_exists mll_file then
    mll_file
  else
  let ml_file = file ^ ".ml" in
  if Sys.file_exists ml_file then
    ml_file
  else
  let mli_file = file ^ ".mli" in
  if Sys.file_exists mli_file then
    mli_file
  else
    failwith "ocp-annot: could not locate associated file"


let query_file_long_ident c path =
  let file = AnnotQueryMisc.find_by_path c
    (AnnotMisc.find_max_rec c ".") find_file_for_ident path in
  let v = Record
        [ "ident", String path;
          "file", String file ]
  in
  Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

let query_local_uses_long_ident c path =
  let uses = ref [] in
  AnnotMisc.iter_files "." (fun file ->
    if Filename.check_suffix file ".annot" then
      AnnotQueryMisc.iter_idents file
        (fun loc ident ->
          match ident with
          | ExtRef ident when ident = path ->
            begin
              match AnnotParser.LOC.begin_pos loc with
              | Some pos ->
                uses := (List [String (AnnotParser.LOC.file loc);
                               AnnotMisc.pos_of_pos pos]) :: !uses
              | None -> ()
            end
          | _ -> ()
        )
  );
  let v = Record
    [ "ident", String path;
      "uses", List !uses ]
  in
  Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

(************************************************************************)
(* Query alternate file (interface/implementation)                      *)

let query_alternate_file c filename =
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
  Printf.printf "%s\n%!" (!AnnotMisc.output_function (Record infos))




let emacs_mode () =
  AnnotMisc.output_function := AnnotEmacs.string_of_output;
  AnnotMisc.output_config := AnnotEmacs.output_config

let json_mode () =
  AnnotMisc.output_function := AnnotJSON.string_of_output

let output_config () = !AnnotMisc.output_config ()

let wrap c f arg =
  try
    begin match c.query_chdir with
    | None -> () | Some dir -> Unix.chdir dir
    end;
    f c arg
  with exn ->
    let bt = Printexc.get_backtrace () in
    let oc =
      open_out_gen [ Open_creat; Open_append ] 0o644 "/tmp/ocp-annot.log" in
    Printf.fprintf oc
      "'%s'\n" (String.concat "' '"
                  (Array.to_list Sys.argv));
    Printf.fprintf oc "dir: %S\n" (Sys.getcwd ());
    Printf.fprintf oc
      "Error: exception %s\n" (Printexc.to_string exn);
    if bt <> "" then
      Printf.fprintf oc "Backtrace:\n%s\n%!" bt;
    close_out oc;
    let infos = [ "error",
                  String (Printf.sprintf "Error: exception %s"
                            (Printexc.to_string exn)) ] in
    let v = Record infos in
    Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

let () =
  emacs_mode () (* default ! *)
