# Scylla, a tool for translating ultra-regular C code to Safe Rust

Scylla consumes C code via libclang, and emits Rust code, relying on the compilation strategies
outlined in our [paper](https://arxiv.org/pdf/2412.15042). Scylla relies on the
[KaRaMeL](https://github.com/FStarLang/karamel/) compiler to perform the necessary intermediary
rewritings and to convert its C-like internal representation to Rust.

```
   libclang            KaRaMeL
C ----------> Scylla -----------> Rust
```

## Status

For now, this requires code to be *extremely* regular in order to be eligible for translation. Such
code can be found in the [HACL\*](https://github.com/hacl-star/hacl-star/) and
[EverParse](https://github.com/project-everest/everparse/) verified C libraries.

So far, our only demo is the chacha20 algorithm from HACL\*.

### Previous incarnation

An earlier version of this toolchain directly consumes the intermediary Mini-C representation used
to produce HACL\* and Everparse.

```
                                         KaRaMeL
  Low*      F* extraction               toolchain
verified  ----------------> Mini-C AST -----------> Rust
 source
```

This earlier version can extract all of HACL\* to Rust. This project is about doing the same thing
but with an actual C frontend.

## Setup

This project relies on an OCaml toolchain. In a Linux environment:

```bash
sudo apt install opam cargo
opam init
# follow instructions, reload shell etc.
git clone git@github.com:FStarLang/karamel
# LINE BELOW TEMPORARY
(cd karamel && git checkout afromher_rust)
(cd karamel && make lib/AutoConfig.ml)
(cd karamel && opam install --deps-only .)
git clone git@github.com:aeneasverif/scylla
cd scylla
(cd lib && ln -s ../../karamel/lib krml)
opam install clangml refl
make test
```
