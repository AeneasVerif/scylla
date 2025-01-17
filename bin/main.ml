let () =
  let _c_files = Scylla.ClangToAst.read_file () in
  Printf.printf "Translated test.rs\n"
