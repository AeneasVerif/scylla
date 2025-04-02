let () =
  let usage =
    Printf.sprintf
      {|Scylla: from (very structured) C to Rust

Usage: %s [OPTIONS] FILES

FILES are .c files, where declarations are potentially decorated with Scylla-specific attributes.

Supported options:|}
      Sys.argv.(0)
  in
  let prepend_csv l s =
    l := Krml.KString.split_on_char ',' s @ !l
  in
  let append_csv l s =
    l := !l @ Krml.KString.split_on_char ',' s
  in
  let debug = prepend_csv Krml.Options.debug_modules in
  (* Order of compiler arguments matters, so we append here *)
  let ccopts = append_csv Scylla.Options.ccopts in
  let spec =
    [
      "--debug", Arg.String debug, " debug options, to be passed to krml";
      "--output", Arg.Set_string Krml.Options.tmpdir, " output directory in which to write files";
      "--ccopts", Arg.String ccopts, " options to be passed to clang, separated by commas";
    ]
  in
  let spec = Arg.align spec in
  let files = ref [] in
  let fatal_error fmt =
    Printf.ksprintf (fun s ->
        print_endline s;
        exit 255) fmt
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

  let files = !files in

  if files = [] then
    fatal_error "%s" (Arg.usage_string spec usage);

  Krml.Options.(warn_error := !warn_error ^ "-6");
  Krml.Warn.parse_warn_error !Krml.Options.warn_error;

  let boxed_types, files = List.fold_left_map (fun acc (f: string) ->
    let boxed_types, files = Scylla.ClangToAst.translate_compil_unit (Scylla.ClangToAst.read_file f) f in
    Krml.AstToMiniRust.LidSet.union acc boxed_types, files
  ) Krml.AstToMiniRust.LidSet.empty files in
  let files = List.concat files in

  if Krml.Options.debug "ClangToAst" then begin
    Format.printf "%!";
    Format.eprintf "%!";
    Krml.(Print.print (PPrint.(PrintAst.print_files files ^^ hardline)))
  end;

  let had_errors, files = Krml.Checker.check_everything ~warn:true files in
  if had_errors then
    fatal_error "%s:%d: input Ast is ill-typed, aborting" __FILE__ __LINE__;

  let files = Krml.Bundles.topological_sort files in
  let files = Krml.Simplify.sequence_to_let#visit_files () files in
  let files = Krml.AstToMiniRust.translate_files_with_boxed_types files boxed_types in
  let files = Krml.OptimizeMiniRust.cleanup_minirust files in
  let files = Krml.OptimizeMiniRust.infer_mut_borrows files in
  let files = Krml.OptimizeMiniRust.simplify_minirust files in
  debug "rs-filenames";
  Krml.OutputRust.write_all files
