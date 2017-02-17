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

let string_of_output v =
  let b = Buffer.create 100 in
  let rec iter v =
    match v with
    | Int n -> Printf.bprintf b "%d" n
    | String s -> Printf.bprintf b "%S" s
    | Record labels ->
      Printf.bprintf b "{";
      List.iteri (fun i (label, v) ->
        if i > 0 then Buffer.add_char b ',';
        Printf.bprintf b "\"%s\": " label;
        iter v;
      ) labels;
      Printf.bprintf b "}";
    | List values ->
      Printf.bprintf b "[ ";
      List.iteri (fun i v ->
        if i > 0 then Buffer.add_string b ", ";
        iter v;
      ) values;
      Printf.bprintf b "]";
  in
  iter v;
  Buffer.contents b
