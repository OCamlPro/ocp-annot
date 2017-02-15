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

val emacs_mode : unit -> unit
val json_mode : unit -> unit

val query_info_file_pos : string -> unit
val query_jump_file_pos : string -> unit
val query_jump_long_ident : string -> unit
val query_alternate_file : string -> unit

val output_config : unit -> unit

val wrap : ('a -> unit) -> 'a -> unit
