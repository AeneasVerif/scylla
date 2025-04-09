(* On MacOS, C compilation often relies on a SDK, where parts of the stdlib
    is located *)
let get_sdkroot () =
  (* TODO: Is there something similar on Linux, or is the stdlib included in
     the Clang default include directories? *)
  try Unix.getenv "SDKROOT" |> String.split_on_char ':' with Not_found -> []

let () =
  let usage =
    Printf.sprintf
      {|Scylla: from (very structured) C to Rust

Usage: %s [OPTIONS] FILES

FILES are .c files, where declarations are potentially decorated with Scylla-specific attributes.

Supported options:|}
      Sys.argv.(0)
  in
  let prepend r = fun s -> r := s :: !r in
  let prepend_csv l s = l := Krml.KString.split_on_char ',' s @ !l in
  let append_csv l s = l := !l @ Krml.KString.split_on_char ',' s in
  let debug = prepend_csv Krml.Options.debug_modules in
  (* Order of compiler arguments matters, so we append here *)
  let ccopts = append_csv Scylla.Options.ccopts in
  let parse_bundle s =
    let open Krml.Bundle in
    let apis, pats, attrs = Krml.Parsers.bundle s in
    apis, List.map (function
      | Lid ([], m) -> Module [m]
      | _ -> failwith "no dots in C to Rust bundles"
    ) pats, attrs
  in
  let spec =
    [
      "--debug", Arg.String debug, " debug options, to be passed to krml";
      "--output", Arg.Set_string Krml.Options.tmpdir, " output directory in which to write files";
      "--ccopts", Arg.String ccopts, " options to be passed to clang, separated by commas";
      "--bundle", Arg.String (fun s -> prepend Krml.Options.bundle (parse_bundle s)), " \
        see krml documentation";
      ( "--errors_as_warnings",
        Arg.Clear Scylla.Options.fatal_errors,
        " unsupported declarations are a fatal error" );
    ]
  in
  let spec = Arg.align spec in
  let files = ref [] in
  let fatal_error fmt =
    Printf.ksprintf
      (fun s ->
        print_endline s;
        exit 255)
      fmt
  in
  let anon_fun f =
    if Filename.check_suffix f ".c" || Filename.check_suffix f ".h" then
      files := f :: !files
    else
      fatal_error "Unknown file extension for %s" f
  in
  begin
    try Arg.parse spec anon_fun usage
    with e ->
      Printf.printf "Error parsing command-line: %s\n%s\n" (Printexc.get_backtrace ())
        (Printexc.to_string e);
      fatal_error "Incorrect invocation, was: %s\n" (String.concat "␣" (Array.to_list Sys.argv))
  end;

  let command_line_args = !files in

  if command_line_args = [] then
    fatal_error "%s" (Arg.usage_string spec usage);

  Krml.Options.(warn_error := !warn_error ^ "-6");
  Krml.Warn.parse_warn_error !Krml.Options.warn_error;

  let files = List.map Scylla.ClangToAst.read_file command_line_args in
  let deduped_files = Scylla.ClangToAst.pick_most_suitable files in
  let lib_dirs = get_sdkroot () @ Clang.default_include_directories () in
  let files = Scylla.ClangToAst.split_into_files lib_dirs deduped_files in
  Scylla.ClangToAst.fill_type_maps deduped_files;
  let boxed_types, files = Scylla.ClangToAst.translate_compil_units files command_line_args in

  let files = Krml.Builtin.lowstar_ignore :: files in

  (* Makes debugging the checker messages horrible, otherwise *)
  let files = Krml.Simplify.let_to_sequence#visit_files () files in

  if Krml.Options.debug "ClangToAst" then begin
    Format.printf "@.%!";
    Format.eprintf "@.%!";
    Krml.(Print.print PPrint.(PrintAst.print_files files ^^ hardline));
    Format.printf "@.%!";
    Format.eprintf "@.%!"
  end;

  let had_errors, files = Krml.Checker.check_everything ~warn:true files in
  if had_errors then
    fatal_error "%s:%d: input Ast is ill-typed, aborting" __FILE__ __LINE__;

  let files = Krml.Bundles.topological_sort files in
  let files = Krml.Bundles.make_bundles files in

  let files = Krml.Simplify.sequence_to_let#visit_files () files in
  let files = Scylla.Simplify.remove_addrof_index#visit_files () files in

  let files = Krml.AstToMiniRust.translate_files_with_boxed_types files boxed_types in
  let files = Krml.OptimizeMiniRust.cleanup_minirust files in
  let files = Krml.OptimizeMiniRust.infer_mut_borrows files in
  let files = Krml.OptimizeMiniRust.simplify_minirust files in
  debug "rs-filenames";
  Krml.OutputRust.write_all files
