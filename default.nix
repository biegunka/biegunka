{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  drv = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./biegunka.nix {};
in
  nixpkgs.stdenv.lib.overrideDerivation drv (biegunka: {
    buildInputs = biegunka.buildInputs ++ [ nixpkgs.pkgs.git ];
  })
