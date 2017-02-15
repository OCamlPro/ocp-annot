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
open AnnotWatch.TYPES
open AnnotParser.TYPES


let quiet = ref false
let verbose = ref false
let project_includes = ref []
let watch_delay = ref 2

let watch () =
  AnnotWatch.watch {
    quiet = !quiet;
    project_includes = !project_includes;
    verbose = !verbose;
    delay = !watch_delay;
  }

let just_parse filename =
        Printf.eprintf "Parse .annot file %S\n%!" filename;
      let _locs = AnnotParser.parse_file filename in
      Printf.eprintf "OK !\n%!"


exception AmbiguousAction
let action = ref None
let set_action f arg =
  match !action with
  | None -> action := Some (fun () -> f arg)
  | Some _ -> raise AmbiguousAction

let set_query_action f arg = set_action (AnnotQuery.wrap f) arg

let arg_list_query = [

  "--emacs", Arg.Unit AnnotQuery.emacs_mode,
  " Output for Emacs";
  "--json", Arg.Unit AnnotQuery.json_mode,
  " Output in JSON";

  "--query-info-file-pos",
  Arg.String  (set_query_action AnnotQuery.query_info_file_pos),
  "POSITION Query type at pos {action}";

  "--query-jump-file-pos",
  Arg.String  (set_query_action AnnotQuery.query_jump_file_pos),
  "POSITION Query jump info at pos {action}";

  "--query-jump-long-ident",
  Arg.String (set_query_action AnnotQuery.query_jump_long_ident),
  "LIDENT Query jump info at pos {action}";

  "--query-alternate-file",
  Arg.String (set_query_action AnnotQuery.query_alternate_file),
  "FILE Find interface/implementation {action}";

  "--output-config",
  Arg.Unit (set_action AnnotQuery.output_config),
  " Output editor config (Emacs) {action}";
]

let arg_list_watch = [

  "--watch", Arg.Unit (set_action watch),
  " Watch dir to translate .cmt to .annot files {action}";

  "-I", Arg.String (fun s ->
    project_includes := s :: !project_includes),
  "DIR Read .cmi files from here";

  "--clean", Arg.Unit (set_action AnnotWatch.clean),
  " Clear all .annot files {action}";

  "--watch-delay", Arg.Int ((:=) watch_delay),
  Printf.sprintf "SECS Delay in seconds between scans (default %d)"
    !watch_delay;

]

let arg_list_misc = [
  "--verbose", Arg.Set verbose,
  " Verbose mode";
  "--quiet", Arg.Set quiet,
  " Quiet mode (equivalent to -q)";
  "-q", Arg.Set quiet,
  " Quiet mode (equivalent to --quiet)";

  "--just-parse", Arg.String (set_action just_parse),
  "FILE Just parse .annot file FILE for testing {action}";
]

let print_arg_usage arg_list msg =
  Printf.printf "%s\n" msg;
  List.iter (fun (key, _, doc) ->
    Printf.printf "  %s %s\n" key doc
  ) (Arg.align arg_list);
  Printf.printf "%!"


let main () =
  let arg_usage = "ocp-annot [OPTIONS]\n  Use .annot files" in
  let arg_help () =
    print_arg_usage (
      arg_list_misc @
        ["--help", Arg.Unit (fun () -> ()), " Display help on ocp-annot" ]
    )
      "\nocp-annot [OPTIONS]\n\n\
ocp-annot provides two main functionalities:\n\
* lookup information in .annot files (typically for editors)\n\
* watch a project to translate updated .cmt files to .annot files\n\
\n\
Generic options:";
    print_arg_usage arg_list_query "\nLookup mode options:";
    Printf.printf "\n  POSITION is either FILE:LINE:LINEPOS or FILE:CHARPOS\n";
    print_arg_usage arg_list_watch "\nWatch mode options:";
    Printf.printf "\n%!"
  in

  let arg_error () = arg_help (); exit 2 in
  let arg_help () = arg_help (); exit 0 in

  let arg_list_help = [
    "-help", Arg.Unit arg_help, " Display help on ocp-annot";
    "--help", Arg.Unit arg_help, " Display help on ocp-annot";
  ] in

  let arg_list = Arg.align (arg_list_query
                            @ arg_list_watch
                            @ arg_list_misc
                            @ arg_list_help)
  in

  begin try
          Arg.parse arg_list (fun s ->
            Printf.eprintf "Error: unexpected argument %S\n%!" s;
            arg_error ();
          ) arg_usage;
    with AmbiguousAction ->
      Printf.eprintf  "Error: ambiguous command with two actions\n%!";
      arg_error ()
  end;
  match !action with
  | None ->
    Printf.eprintf "Error: no action specified\n%!";
    arg_error ()
  | Some action -> action ()


let () =
  try
    main ()
  with exn ->
    (* The same code exists in annotQuery.ml, we should factorize at
       some point *)
    let bt = Printexc.get_backtrace () in
    let oc =
      open_out_gen [ Open_creat; Open_append ] 0o644 "/tmp/ocp-annot.log" in
    Printf.fprintf oc
      "'%s'\n" (String.concat "' '"
                  (Array.to_list Sys.argv));
    Printf.fprintf oc
      "Error: exception %s\n" (Printexc.to_string exn);
    if bt <> "" then
      Printf.fprintf oc "Backtrace:\n%s\n%!" bt;
    close_out oc
