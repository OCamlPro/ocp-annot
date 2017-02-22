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
  | _, [] -> failwith "ocp-annot: no info found at file pos"
  | annot_file, infos ->
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
                match AnnotParser.parse_ident annot_file ident with
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

let find_jump_for_ident annot_filename idents =
  let annot_file = AnnotParser.parse_file annot_filename in
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
      match AnnotParser.parse_ident annot_file ident with
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
              Filename.concat (Filename.dirname annot_filename)
                loc.pos_file.file_basename in
            [
              "file", String ml_file;
              "pos", AnnotMisc.pos_of_pos loc;
            ]
          | Unlocated -> iter locs
          end
        | _ -> iter_infos loc infos locs
  in
  iter annot_file.annot_infos

let find_ident_at_pos c file_pos =
  match AnnotQueryMisc.query_at_pos file_pos with
  | _, [] -> failwith "ocp-annot: no info found at pos"
  | annot_file, ( _, loc, infos ) :: _ ->
    let rec iter infos =
      match infos with
      | [] -> failwith "ocp-annot: info contains no reference to definition"
      | info :: infos ->
        match info with
        | Type _ -> iter infos
        | Ident ident ->
          annot_file, loc, AnnotParser.parse_ident annot_file ident
    in
    iter infos

let query_jump_file_pos c file_pos =
  let (_, loc, ident) = find_ident_at_pos c file_pos in
  let v = match ident with
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
    Printf.printf "%s\n%!" (!AnnotMisc.output_function (Record v))

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

(* Try to find an absolute filename corresponding to an existing file
   for the location of the file *)
let loc_file ?project_dir annot_file loc =
  let ml_file = AnnotParser.LOC.file loc in
  if not (Filename.is_relative ml_file) then
    ml_file
  else
  let annot_dirname = Filename.dirname annot_file in
  let ml_basename = Filename.basename ml_file in
  let ml_filename = Filename.concat annot_dirname ml_basename in
  if Sys.file_exists ml_filename then
    ml_filename
  else
    let ml_dirname = match project_dir with
        None -> Sys.getcwd ()
      | Some dir -> dir
    in
    let ml_filename = Filename.concat ml_dirname ml_basename in
    if Sys.file_exists ml_filename then
      ml_filename
    else
      ml_file

let lident_of_string path =
  let lident_of_file_path file path =
    match AnnotQueryMisc.split_lident path with
    | _,[] ->
        let modname =
          String.capitalize
            (Filename.chop_extension
               (Filename.basename file)) in
        modname, [path]
    | modname, idents ->
      modname, idents
  in
  match OcpString.split path ',' with
  | [_] -> AnnotQueryMisc.split_lident path
  | [file; path] -> lident_of_file_path file path
  | _ -> Printf.kprintf failwith "ocp-annot: cannot extract lident from %S" path

let kind_of_kind = function
  | Def _ -> "def"
  | ExtRef _ -> "ext_ref"
  | IntRef _ -> "int_ref"

let find_uses_long_ident c path =
  let modname, idents = lident_of_string path in
  let path = String.concat "." (modname :: idents) in
  let modname_annot = modname ^ ".annot" in
  let uses = ref [] in
  let project_dir = match AnnotMisc.find_project_root () with
    | None -> Sys.getcwd ()
    | Some (dir,_level) -> dir
  in
  AnnotMisc.iter_files ~skip:AnnotMisc.skip_dirs
    project_dir (fun file ->
      if Filename.check_suffix file ".annot" then
        if String.capitalize (Filename.basename file) = modname_annot then
          let local_ident = OcpList.last idents in
          AnnotQueryMisc.iter_idents file
            (fun loc kind ->
              match kind with
              | Def (ident,_)
              | IntRef (ident,_) when ident = local_ident ->
                begin
                  match AnnotParser.LOC.begin_pos loc with
                  | None -> ()
                  | Some pos ->
                    let ml_file = loc_file ~project_dir file loc in
                    uses := (kind_of_kind kind, ml_file,  pos) :: !uses
                end
              | _ -> ()
            )
        else
      AnnotQueryMisc.iter_idents file
        (fun loc kind ->
          match kind with
          | ExtRef ident when ident = path ->
            begin
              match AnnotParser.LOC.begin_pos loc with
              | Some pos ->
                let ml_file = loc_file ~project_dir file loc in
                uses := (kind_of_kind kind, ml_file, pos) :: !uses
              | None -> ()
            end
          | _ -> ()
        )
  );
  path, !uses

let print_list_of_uses path uses =
  List.iter (fun (kind, ml_file, pos) ->
    let line = pos.pos_line in
    let linepos = pos.pos_linepos in
    Printf.printf "File %S, line %d, characters %d-%d:\n"
      ml_file line linepos linepos;
    Printf.printf "%s occurrence of %S\n%!"
      (match kind with
        "def" -> "Definition"
      | "int_ref" -> "Internal"
      | "ext_ref" -> "External"
      | _ -> kind
      ) path
  ) uses;
  Printf.printf "%!"

let output_list_of_uses path uses =
  let uses = List.map (fun (kind, ml_file, pos) ->
    List [String ml_file; AnnotMisc.pos_of_pos pos]
  ) uses in
  let v = Record
    [ "ident", String path;
      "uses", List uses;
    ]
  in
  Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

let query_uses_long_ident c path =
  let path, uses = find_uses_long_ident c path in
  output_list_of_uses path uses

let query_occur_long_ident c path =
  let path, uses = find_uses_long_ident c path in
  print_list_of_uses path uses

let find_uses_lident annot_file lident = []
let find_uses_by_def annot_file id def_loc =
  let project_dir = match AnnotMisc.find_project_root () with
    | None -> Sys.getcwd ()
    | Some (dir,_level) -> dir
  in
  let uses = ref [ ] in (* kind, string, position *)
  let is_def_loc kind loc =
    match AnnotParser.LOC.begin_pos loc, AnnotParser.LOC.end_pos loc with
    | Some begin_pos, Some end_pos ->
      let left = AnnotParser.LOC.includes def_loc begin_pos in
      let right = AnnotParser.LOC.includes def_loc end_pos in
      left && right
    | _ -> false
  in
  let add_use kind loc use_loc ident =
    if is_def_loc kind loc && ident = id then
      match AnnotParser.LOC.begin_pos use_loc with
      | None -> ()
      | Some pos ->
        let ml_file = loc_file ~project_dir annot_file.annot_filename use_loc in
        uses := (kind, ml_file, pos) :: !uses
  in
  List.iter (fun (loc, infos) ->
    List.iter (fun info ->
      match info with
      | Type _ -> ()
      | Ident ident ->
        let ident = AnnotParser.parse_ident annot_file ident in
        match ident with
        | IntRef (ident, def_loc) ->
          add_use "int_ref" def_loc loc ident
        | Def (ident, scope) -> add_use "def" loc loc ident
        | _ -> ()
    ) infos
  ) annot_file.annot_infos;
  !uses

let query_local_uses_pos c file_pos =
  let (annot_file, loc, ident) = find_ident_at_pos c file_pos in
  let path, uses = match ident with
    | ExtRef lident ->
      lident, find_uses_lident annot_file lident
    | Def (ident, _scope) ->
      ident, find_uses_by_def annot_file ident loc
    | IntRef (ident, def_loc) ->
      ident, find_uses_by_def annot_file ident def_loc
  in
  output_list_of_uses path uses

let query_local_occur_pos c file_pos =
  let (annot_file, loc, ident) = find_ident_at_pos c file_pos in
  let path, uses = match ident with
    | ExtRef lident ->
      lident, find_uses_lident annot_file lident
    | Def (ident, _scope) ->
      ident, find_uses_by_def annot_file ident loc
    | IntRef (ident, def_loc) ->
      ident, find_uses_by_def annot_file ident def_loc
  in
  print_list_of_uses path uses

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
    AnnotMisc.log_exn exn;
    let infos = [ "error",
                  String (Printf.sprintf "Error: exception %s"
                            (Printexc.to_string exn)) ] in
    let v = Record infos in
    Printf.printf "%s\n%!" (!AnnotMisc.output_function v)

let () =
  emacs_mode () (* default ! *)
