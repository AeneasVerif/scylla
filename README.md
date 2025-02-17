# Scylla, a tool for translating ultra-regular C code to Safe Rust

⚠️⚠️⚠️ THIS IS VERY EXPERIMENTAL AND IS AN ONGOING DEVELOPMENT ⚠️⚠️⚠️

## Background (Low* to Rust)

Low* is a DSL of [F\*] that compiles to C via the
[KaRaMeL] compiler. Low* was used to author the [HACL\*] verified
cryptographic library and [EverParse] verified parsers library.

The [earlier version](https://github.com/FStarLang/karamel/blob/master/lib/AstToMiniRust.ml) of this
toolchain directly consumes the intermediary Low* representation.

[F\*]: https://fstar-lang.org
[KaRaMeL]: https://github.com/FStarLang/karamel
[HACL\*]: https://github.com/hacl-star/hacl-star
[EverParse]: https://github.com/project-everest/everparse/

```
                                         KaRaMeL
  Low*      F* extraction               toolchain
verified  ----------------> Mini-C AST -----------> Rust
 source
```

This earlier version [can extract all of
HACL\*](https://github.com/hacl-star/hacl-star/tree/afromher_rs/dist/rs/src) to Rust. This is what
we described in our [preprint].

[preprint]: https://arxiv.org/pdf/2412.15042

## This project (Scylla: a small subset of C to Rust)

Scylla is about doing the same thing but with an actual C frontend.

Scylla consumes C code via libclang, and emits Rust code, relying on the same compilation strategies
outlined in our [preprint]. Scylla relies on the [KaRaMeL] compiler to perform the necessary
intermediary rewritings and to convert its C-like internal representation to Rust.

```
   Scylla +                KaRaMeL
   libclang               toolchain
C ----------> Mini-C AST -----------> Rust
```

## Status

A ton of features are missing and many constructs from libclang are not handled; this project, for
now, is about assessing whether our methodology can still function with an actual C frontend.

Scylla requires code to be *extremely* regular in order to be eligible for translation. Such
code can be found in the [HACL\*] and
[EverParse] verified C libraries.

So far, our demo contains a handful of files from HACL\*.

## Setup

This project relies on an OCaml toolchain. We have tested this code on Mac OS 14 "Sonoma" and Ubuntu
22.04, with LLVM versions 14 and 15. The instructions below assume you have not cloned this
repository yet.

```bash
# Step 1: install OCaml environment. Follow instructions, reload your shell, and make sure 
# `eval $(opam env)` has been suitably added to your shell profile.
sudo apt install opam cargo # or brew on OSX
opam init

# Step 2: clone the two repositories side by side
git clone git@github.com:FStarLang/karamel
git clone git@github.com:aeneasverif/scylla

# Step 3: install required OCaml packages. Note: the invocation for karamel might fail, in which
# case you want to install all the packages in the `depends` field of karamel.opam except fstar. At
# the time of writing, this means typing:
# opam install ocamlfind batteries zarith stdint yojson ocamlbuild fileutils menhir pprint ulex process fix visitors wasm ppx_deriving ppx_deriving_yojson uucp
(cd karamel && opam install --deps-only .)
(cd scylla && opam install clangml refl sedlex visitors)

# Step 4: misc. setup steps
(cd karamel && make lib/AutoConfig.ml)
(cd scylla/lib && ln -s ../../karamel/lib krml)

# Step 5: ready!
cd scylla
make test
```
