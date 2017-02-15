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

module TYPES : sig

  type file = {
    file_name : string;
    (* we over-approximate that two files are equal if they have the
       same basename, and are included in the same file... *)
    file_basename : string;
  }

  type position = {
    pos_file: file;
    pos_line: int;
    pos_linepos:  int; (* pos between char and beginning of line *)
    pos_pos : int; (* usually not meaningful, especially if the file
                      contains #loc directives *)
  }

  type location =
    Bounded of position * position
  | Unbounded of position
  | Uncomparable of position * position
  | Unlocated

  type kind =
    Type of string list
  | Ident of string

  type ident =
    Def of string * location (* location of scope, end_pos = -1 => unlimited *)
  | ExtRef of string
  | IntRef of string * location (* location of definition *)

  type annot_file = {
    annot_basenames : file StringMap.t;
    annot_infos : (location * kind list) list;
  }

end

module POS : sig
  val to_string : TYPES.position -> string
  val comparable : TYPES.position -> TYPES.position -> bool
  val gt : TYPES.position -> TYPES.position -> bool
  val gte : TYPES.position -> TYPES.position -> bool
  val lte : TYPES.position -> TYPES.position -> bool
  val lt: TYPES.position -> TYPES.position -> bool
  val approx : TYPES.position -> int
end

module LOC : sig
  val to_string : TYPES.location -> string
  val approx : TYPES.location -> int
  val includes : TYPES.location -> TYPES.position -> bool
  val file : TYPES.location -> string
  val begin_pos : TYPES.location -> TYPES.position option
  val end_pos : TYPES.location -> TYPES.position option
end

module KIND : sig
  val to_string : TYPES.kind -> string
end

(* [parse_file filename] will parse [filename] as a .annot file generated
   by OCaml. *)
val parse_file :  string -> TYPES.annot_file

(* [parse_ident s] will parse [s] as the single-line following an
   "ident(" line in a .annot file. It is used to handle the string argument
   of the [ident] constructor of the [kind] type. *)
val parse_ident : string -> TYPES.ident
