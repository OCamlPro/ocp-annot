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

val find_by_path :
  AnnotQueryTypes.config ->
  int -> (* max_rec *)
  (string -> string list -> 'a) ->
  string -> (* LIDENT *)
  'a

val query_at_pos :
  string -> (* POSITION *)
  (int * AnnotParser.TYPES.location * AnnotParser.TYPES.kind list) list

val iter_idents :
  string -> (* annot_filename *)
  (AnnotParser.TYPES.location -> AnnotParser.TYPES.ident -> unit) ->
  unit

val split_lident : string -> string * string list
