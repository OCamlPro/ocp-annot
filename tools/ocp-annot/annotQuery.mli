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

module TYPES : sig

  type config = {
    max_rec : int;
    query_chdir : string option;
    timeout : float;
  }

end

val emacs_mode : unit -> unit
val json_mode : unit -> unit

val query_info_file_pos : TYPES.config -> string -> unit
val query_jump_file_pos : TYPES.config -> string -> unit
val query_jump_long_ident : TYPES.config -> string -> unit
val query_alternate_file : TYPES.config -> string -> unit
val query_file_long_ident : TYPES.config -> string -> unit
val query_uses_long_ident : TYPES.config -> string -> unit
val query_occurrences_long_ident : TYPES.config -> string -> unit

val output_config : unit -> unit

val wrap : TYPES.config -> (TYPES.config -> 'a -> unit) -> 'a -> unit
