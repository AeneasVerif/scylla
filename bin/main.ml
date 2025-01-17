let () =
  let ast = Scylla.ClangToAst.read_file "test/test.c" in
  let files = Scylla.ClangToAst.translate_compil_unit ast in
  let files = Krml.Simplify.sequence_to_let#visit_files () files in
  let files = Krml.AstToMiniRust.translate_files files in
  let files = Krml.OptimizeMiniRust.cleanup_minirust files in
  let files = Krml.OptimizeMiniRust.infer_mut_borrows files in
  let files = Krml.OptimizeMiniRust.simplify_minirust files in
  Krml.OutputRust.write_all files;
  Printf.printf "Translated test.rs\n"
