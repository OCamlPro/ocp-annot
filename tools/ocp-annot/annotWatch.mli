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
  project_includes : string list;
  verbose : bool;
  quiet : bool;
  watch_delay : int;
  watch_chdir : string option;
}

end

val clean : unit -> unit
val watch : TYPES.config -> 'a
val check : unit -> unit
