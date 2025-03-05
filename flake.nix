{
  inputs = {
    flake-utils.follows = "karamel/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    karamel.url = "github:FStarLang/karamel";
    karamel.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, ... } @ inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nix-filter = inputs.nix-filter.lib;

        overlays = [
          # Rust toolchain
          (import inputs.rust-overlay)
          # Use ocaml 5
          (final: super: { ocamlPackages = final.ocaml-ng.ocamlPackages_5_0; })
        ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };

        # LLVM version to use for clang bindings
        llvmPackages = pkgs.llvmPackages_15;
        rustToolchain = pkgs.rust-bin.stable.latest.default;
        craneLib = (inputs.crane.mkLib pkgs).overrideToolchain rustToolchain;

        karamel = inputs.karamel.packages.${system}.default.override { ocamlPackages = pkgs.ocamlPackages; };

        # We have to package a bunch of ocaml packages ourselves because they're not in nixpkgs.
        extraOcamlPackages = rec {
          ocaml-migrate-parsetree = pkgs.callPackage
            ({ fetchFromGitHub
             , ocamlPackages
             }:
              ocamlPackages.buildDunePackage rec {
                pname = "ocaml-migrate-parsetree";
                version = "5cb1140";
                src = fetchFromGitHub {
                  owner = "ocaml-ppx";
                  repo = pname;
                  rev = version;
                  hash = "sha256-OwluJOG6flUesb1LefGKB7miBagorqMEGpgOzxZZios=";
                };
                propagatedBuildInputs = [
                ];
              })
            { };

          metapp = pkgs.callPackage
            ({ fetchFromGitHub
             , ocamlPackages
             }:
              ocamlPackages.buildDunePackage rec {
                pname = "metapp";
                version = "26e2071";
                src = fetchFromGitHub {
                  owner = "ocamllibs";
                  repo = pname;
                  rev = version;
                  hash = "sha256-5h7uFPbVtp8A0PbTc+JGlv8H/6xQ1QjwNeRiImAHcfU=";
                };
                propagatedBuildInputs = [
                  ocamlPackages.findlib
                  ocamlPackages.stdcompat
                  ocamlPackages.ppxlib
                ];
              })
            { };

          metaquot = pkgs.callPackage
            ({ fetchFromGitHub
             , ocamlPackages
             , metapp
             }:
              ocamlPackages.buildDunePackage rec {
                pname = "metaquot";
                version = "e19ef99";
                src = fetchFromGitHub {
                  owner = "ocamllibs";
                  repo = pname;
                  rev = version;
                  hash = "sha256-laGPCOqcnqO9obh7tL8mqIZl9XfMZEKsx6z/yWgmcTg=";
                };
                propagatedBuildInputs = [
                  ocamlPackages.stdcompat
                  metapp
                ];
              })
            { inherit metapp; };

          traverse = pkgs.callPackage
            ({ fetchFromGitHub
             , ocamlPackages
             , metapp
             }:
              ocamlPackages.buildDunePackage rec {
                pname = "traverse";
                version = "9d03cf1";
                src = fetchFromGitHub {
                  owner = "ocamllibs";
                  repo = pname;
                  rev = version;
                  hash = "sha256-sDkdG6dBSScpmTr/7QlR2G3/Mkgp/JfaJWMKymesPC8=";
                };
                propagatedBuildInputs = [
                  metapp
                ];
              })
            { inherit metapp; };

          refl = pkgs.callPackage
            ({ fetchFromGitHub
             , ocamlPackages
             , traverse
             , metaquot
             }:
              ocamlPackages.buildDunePackage rec {
                pname = "refl";
                version = "e79345b0";
                src = fetchFromGitHub {
                  owner = "ocamllibs";
                  repo = pname;
                  rev = version;
                  hash = "sha256-aRyx58CPxcTkQa+s+ZL61Xfwcdn8g2GW+uOcCQR7mxY=";
                };
                propagatedBuildInputs = [
                  ocamlPackages.fix
                  traverse
                  metaquot
                ];
              })
            { inherit traverse metaquot; };

          clangml = pkgs.callPackage
            ({ lib
             , fetchFromGitHub
             , ocamlPackages
             , libclang
             , libllvm
             , which
             , refl
             , ocaml-migrate-parsetree
             }:
              ocamlPackages.buildDunePackage rec {
                pname = "clangml";
                version = "v4.8.0";

                src = fetchFromGitHub {
                  owner = "ocamllibs";
                  repo = pname;
                  rev = version;
                  hash = "sha256-I0yHmWSgAzGu6F8OcFc+ZFa5DRwDUJJnEgLnuZbly94=";
                };

                nativeBuildInputs = [ libllvm which ];
                propagatedBuildInputs = [
                  libllvm
                  libclang
                  ocamlPackages.stdcompat
                  ocamlPackages.ppxlib
                  refl
                  ocaml-migrate-parsetree
                ];
              })
            {
              inherit refl ocaml-migrate-parsetree;
              # Override to use our chosen llvm version.
              libclang = llvmPackages.libclang;
              libllvm = llvmPackages.libllvm;
            };
        };

        scylla = pkgs.callPackage
          ({ version
           , ocamlPackages
           , libclang
           , krml
           , clangml
           , clang-tools
           }:
            ocamlPackages.buildDunePackage {
              pname = "scylla";
              inherit version;
              src = nix-filter {
                root = ./.;
                exclude = [
                  "flake.nix"
                ];
              };
              buildInputs = [
                libclang
                clang-tools
              ];
              propagatedBuildInputs = [
                krml
                clangml
              ];
              preBuild = ''
                make lib/DataModel.ml
              '';
            })
          {
            version = self.rev or "dirty";
            krml = karamel.passthru.lib;
            # Provide this package which isn't in nixpkgs.
            clangml = extraOcamlPackages.clangml;
            # Override to use our chosen llvm version.
            libclang = llvmPackages.libclang;
          };

        # Regenerate the rust files and check that they match the committed files.
        scylla-generate-rust-files = llvmPackages.stdenv.mkDerivation {
          name = "scylla-generate-rust-files";
          src = nix-filter {
            root = ./.;
            include = [
              "Makefile"
              "out"
              "rs"
              "test"
            ];
          };
          # Unsure why adding these as `buildInputs` didn't work, but at least this works.
          C_INCLUDE_PATH = "${llvmPackages.clang}/resource-root/include:${pkgs.glibc.dev}/include";
          committed_output = ./out;
          buildPhase = ''
            ln -sf ${scylla}/bin/scylla scylla
            # Don't try to rebuild
            sed -i 's/scylla: build//' Makefile
            make regen-outputs

            # Check that there are no differences between the generated
            # outputs and the committed outputs.
            if diff -rq $committed_output out > /dev/null; then
              echo "Ok: the regenerated files are the same as the checked out files"
            else
              echo "Error: the regenerated files differ from the checked out files"
              diff -ru $committed_output out
              exit 1
            fi
          '';
          installPhase = ''
            mv out $out
          '';
        };

        # Run `cargo test` on the generated rust files.
        scylla-test-rust-files = craneLib.cargoTest rec {
          name = "scylla-test-rust-files";
          src = craneLib.cleanCargoSource "${scylla-generate-rust-files}/hacl";
          cargoArtifacts = craneLib.buildDepsOnly { inherit src; };
          installPhase = ''touch $out'';
        };
      in
      {
        # Provide the custom ocaml packages as well, in case a downstream user
        # wants to override something.
        packages = extraOcamlPackages // { default = scylla; };
        checks = {
          inherit scylla-generate-rust-files scylla-test-rust-files;
        };

        devShells.default = (pkgs.mkShell.override { stdenv = llvmPackages.stdenv; }) {
          OCAMLRUNPARAM = "b"; # Get backtrace on exception
          packages = [
            pkgs.ocamlPackages.ocaml
            # ocaml-lsp's version must match the ocaml version used. Pinning
            # this here to save me a headache.
            pkgs.ocamlPackages.ocaml-lsp
          ];

          # Same as for scylla-generate-rust-files.
          C_INCLUDE_PATH = "${llvmPackages.clang}/resource-root/include:${pkgs.glibc.dev}/include";
          inputsFrom = [
            self.packages.${system}.default
            scylla-generate-rust-files
            scylla-test-rust-files
          ];
        };
      });
}
