{
  inputs = {
    flake-utils.follows = "karamel/flake-utils";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    karamel.url = "github:FStarLang/karamel";
    karamel.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, ... } @ inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
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
      in
      {
        devShells.default = (pkgs.mkShell.override { stdenv = llvmPackages.stdenv; }) {
          # Get backtrace on exception
          # OCAMLRUNPARAM = "b";
          # Unsure why adding these as `buildInputs` didn't work, but at least this works.
          C_INCLUDE_PATH = "${llvmPackages.clang}/resource-root/include:${pkgs.glibc.dev}/include";

          buildInputs = [
            llvmPackages.libclang
            pkgs.clang-tools
            karamel.passthru.lib
            extraOcamlPackages.clangml
          ];

          nativeBuildInputs = [
            rustToolchain
            pkgs.python3
            pkgs.cmake

            pkgs.ocamlPackages.dune_3
            pkgs.ocamlPackages.ocaml
            # ocaml-lsp's version must match the ocaml version used. Pinning
            # this here to save me a headache.
            pkgs.ocamlPackages.ocaml-lsp
          ];
        };
      });
}
