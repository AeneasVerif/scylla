let () =
  let usage =
    Printf.sprintf
      {|Scylla: from (very structured) C to Rust

Usage: %s [OPTIONS] FILES

FILES are .c files potentially decorated with Scylla-specific attributes.

Supported options:|}
      Sys.argv.(0)
  in
  let debug s =
    Krml.Options.debug_modules := Krml.KString.split_on_char ',' s @ !Krml.Options.debug_modules
  in
  let spec =
    [
      "--debug", Arg.String debug, " debug options, to be passed to krml";
      "--output", Arg.Set_string Krml.Options.tmpdir, " output directory in which to write files";
    ]
  in
  let spec = Arg.align spec in
  let files = ref [] in
  let fatal_error fmt =
    Printf.kprintf (fun s ->
        print_endline s;
        exit 255) fmt
  in
  let anon_fun f =
    if Filename.check_suffix f ".c" then
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

  if List.length files > 1 then
    fatal_error "ERROR: cannot currently process more than one C file -- got %d" (List.length files);

  let ast = Scylla.ClangToAst.read_file (Krml.KList.one files) in
  let files = Scylla.ClangToAst.translate_compil_unit ast (Krml.KList.one files) in
  let files = Krml.Bundles.topological_sort files in
  let files = Krml.Simplify.sequence_to_let#visit_files () files in
  let files = Krml.AstToMiniRust.translate_files files in
  let files = Krml.OptimizeMiniRust.cleanup_minirust files in
  let files = Krml.OptimizeMiniRust.infer_mut_borrows files in
  let files = Krml.OptimizeMiniRust.simplify_minirust files in
  debug "rs-filenames";
  Krml.OutputRust.write_all files
