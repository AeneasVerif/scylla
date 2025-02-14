# Scylla, a tool for translating ultra-regular C code to Safe Rust

⚠️⚠️⚠️ THIS IS VERY EXPERIMENTAL AND IS AN ONGOING DEVELOPMENT ⚠️⚠️⚠️

## Background (Low* to Rust)

Low* is a DSL of [F\*] that compiles to C via the
[KaRaMeL] compiler. Low* was used to author the [HACL\*] verified
cryptographic library and [EverParse] verified parsers library.

The [earlier version](https://github.com/FStarLang/karamel/blob/master/lib/AstToMiniRust.ml) of this
toolchain directly consumes the intermediary Low* representation.

F\*: https://fstar-lang.org
KaRaMeL: https://github.com/FStarLang/karamel

```
                                         KaRaMeL
  Low*      F* extraction               toolchain
verified  ----------------> Mini-C AST -----------> Rust
 source
```

This earlier version [can extract all of
HACL\*](https://github.com/hacl-star/hacl-star/tree/afromher_rs/dist/rs/src) to Rust. This is what
we described in our [preprint].

preprint: https://arxiv.org/pdf/2412.15042

## This project (Scylla: a small subset of C to Rust)

Scylla is about doing the same thing but with an actual C frontend.

Scylla consumes C code via libclang, and emits Rust code, relying on the same compilation strategies
outlined in our [preprint]. Scylla relies on the [KaRaMeL] compiler to perform the necessary
intermediary rewritings and to convert its C-like internal representation to Rust.

```
   libclang            KaRaMeL
C ----------> Scylla -----------> Rust
```

## Status

A ton of features are missing and many constructs from libclang are not handled; this project, for
now, is about assessing whether our methodology can still function with an actual C frontend.

Scylla requires code to be *extremely* regular in order to be eligible for translation. Such
code can be found in the [HACL\*] and
[EverParse] verified C libraries.

So far, our only demo is the chacha20 algorithm from HACL\*.

## Setup

This project relies on an OCaml toolchain. In a Linux environment:

```bash
sudo apt install opam cargo
opam init
# follow instructions, reload shell etc.
git clone git@github.com:FStarLang/karamel
(cd karamel && make lib/AutoConfig.ml)
(cd karamel && opam install --deps-only .)
git clone git@github.com:aeneasverif/scylla
cd scylla
(cd lib && ln -s ../../karamel/lib krml)
opam install clangml refl
make test
```
