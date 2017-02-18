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

val has_tool : string -> string option
val pos_of_pos :
  AnnotParser.TYPES.position -> AnnotQueryTypes.output

(* From current directory, find how deep we are in the project
   (probably to bound how up we can backtrack when searching) *)
val find_max_rec : AnnotQueryTypes.config -> string -> int
val find_project_root : unit -> (string * int) option

val use_absolute_pos : bool ref

val output_config : (unit -> unit) ref
val output_function : (AnnotQueryTypes.output -> string) ref

val is_directory : string -> bool
val readdir : string -> string array

val start_time : float
val check_time : AnnotQueryTypes.config -> unit

val skip_dirs : StringCompat.StringSet.t
val iter_files :
  ?skip:StringCompat.StringSet.t ->
  string -> (string -> unit) -> unit

(* [with_log f] calls [f printer] where [printer] prints
   a line in /tmp/ocp-annot.log *)
val with_log : ((string -> unit) -> 'a) -> unit
val log_exn : exn -> unit
