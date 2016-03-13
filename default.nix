{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./biegunka.nix {
  mkDerivation = args: nixpkgs.pkgs.haskell.packages.${compiler}.mkDerivation(args // {
    buildTools = (if args ? buildTools then args.buildTools else []) ++ [ nixpkgs.pkgs.git ];
  });
}
