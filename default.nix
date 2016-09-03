{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./package.nix {
  mkDerivation = args: nixpkgs.pkgs.haskell.packages.${compiler}.mkDerivation(args // {
    buildTools = (if args ? buildTools then args.buildTools else []) ++ [ nixpkgs.pkgs.git ];
  });
}
