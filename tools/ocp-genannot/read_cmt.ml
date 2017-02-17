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

(* Code copied from OCaml 4.05.0 as part of Caml Consortium agreement  *)

let gen_annot = ref false
let gen_ml = ref false
let print_info_arg = ref false
let target_filename = ref None
let save_cmt_info = ref false

let arg_list = Arg.align [
  "-o", Arg.String (fun s -> target_filename := Some s),
    " FILE (or -) : dump to file FILE (or stdout)";
  "-annot", Arg.Set gen_annot, " : generate the corresponding .annot file";
  "-save-cmt-info", Arg.Set save_cmt_info,
  " Encapsulate additional cmt information in annotations";
  "-src", Arg.Set gen_ml,
    " Convert .cmt or .cmti back to source code (without comments)";
  "-info", Arg.Set print_info_arg, " : print information on the file";
#if OCAML_VERSION >= "4.05"
  "-args", Arg.Expand Arg.read_arg,
    " <file> Read additional newline separated command line arguments \n\
    \      from <file>";
  "-args0", Arg.Expand Arg.read_arg0,
    "<file> Read additional NUL separated command line arguments from \n\
    \      <file>";
#endif
  "-I", Arg.String (fun s ->
    Clflags.include_dirs := s :: !Clflags.include_dirs
  ), "DIR Directories for includes";
  ]

let arg_usage =
  "read_cmt [OPTIONS] FILE.cmt : read FILE.cmt and print related information"

let dummy_crc = String.make 32 '-'

let print_info cmt =
  let oc = match !target_filename with
    | None -> stdout
    | Some filename -> open_out filename
  in
  let open Cmt_format in
  Printf.fprintf oc "module name: %s\n" cmt.cmt_modname;
  begin match cmt.cmt_annots with
    Packed (_, list) ->
      Printf.fprintf oc "pack: %s\n" (String.concat " " list)
  | Implementation _ -> Printf.fprintf oc "kind: implementation\n"
  | Interface _ -> Printf.fprintf oc "kind: interface\n"
  | Partial_implementation _ ->
    Printf.fprintf oc "kind: implementation with errors\n"
  | Partial_interface _ -> Printf.fprintf oc "kind: interface with errors\n"
  end;
  Printf.fprintf oc "command: %s\n"
    (String.concat " " (Array.to_list cmt.cmt_args));
  begin match cmt.cmt_sourcefile with
    None -> ()
  | Some name ->
    Printf.fprintf oc "sourcefile: %s\n" name;
  end;
  Printf.fprintf oc "build directory: %s\n" cmt.cmt_builddir;
  List.iter (Printf.fprintf oc "load path: %s\n%!") cmt.cmt_loadpath;
  begin
    match cmt.cmt_source_digest with
      None -> ()
    | Some digest ->
      Printf.fprintf oc "source digest: %s\n" (Digest.to_hex digest);
  end;
  begin
    match cmt.cmt_interface_digest with
      None -> ()
    | Some digest ->
      Printf.fprintf oc "interface digest: %s\n" (Digest.to_hex digest);
  end;
  List.iter (fun (name, crco) ->
    let crc =
      match crco with
        None -> dummy_crc
      | Some crc -> Digest.to_hex crc
    in
    Printf.fprintf oc "import: %s %s\n" name crc;
  ) (List.sort compare cmt.cmt_imports);
  Printf.fprintf oc "%!";
  begin match !target_filename with
  | None -> ()
  | Some _ -> close_out oc
  end;
  ()

let main () =
  Clflags.annotations := true;
#if OCAML_VERSION >= "4.05"
  Arg.parse_expand
#else
  Arg.parse
#endif

  arg_list  (fun filename ->
    if
      Filename.check_suffix filename ".cmt" ||
        Filename.check_suffix filename ".cmti"
    then begin
      Compmisc.init_path false;
      let cmt = Cmt_format.read_cmt filename in
      if !gen_annot then
        Cmt2annot.gen_annot ~save_cmt_info: !save_cmt_info
          !target_filename filename cmt;
      if !gen_ml then Cmt2annot.gen_ml !target_filename filename cmt;
      if !print_info_arg || not (!gen_ml || !gen_annot) then print_info cmt;
    end else begin
      Printf.fprintf stderr
                     "Error: the file's extension must be .cmt or .cmti.\n%!";
      Arg.usage arg_list arg_usage
    end
  ) arg_usage


let () =
  try
    main ()
  with x ->
    Printf.eprintf "Exception in main ()\n%!";
    Location.report_exception Format.err_formatter x;
    Format.fprintf Format.err_formatter "@.";
    exit 2
