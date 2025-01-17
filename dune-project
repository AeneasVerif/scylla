(lang dune 3.7)

(name scylla)

(generate_opam_files true)

(source
 (github AeneasVerif/scylla))

(authors "Aymeric Fromherz")

(maintainers "Aymeric Fromherz")

(license Apache-2.0)

(package
 (name scylla)
 (synopsis "A C to Rust translator")
 (description "C to Rust translator, reusing the KaRaMeL infrastructure")
 (depends
  ocaml
  dune
 )
)

(using menhir 2.1)
