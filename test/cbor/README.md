Scylla command:

OCAMLRUNPARAM=b ./../../scylla --ccopts -DKRML_UNROLL_MAX=0,-isysroot,/opt/homebrew/Cellar/llvm@15/15.0.7/Toolchains/LLVM15.0.7.xctoolchain/,-std=gnu11,-I,.,-I,krml/include,-I,krml/krmllib/dist/generic CBORDet.c --errors_as_warnings --ignore_lib_errors
