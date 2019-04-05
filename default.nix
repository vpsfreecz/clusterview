{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
let
  #data-prometheus = import ../data-prometheus {};
  data-prometheus = nixpkgs.haskell.packages.${compiler}.callPackage ./nix/data-prometheus.nix { };
  pretty-relative-time = nixpkgs.haskell.packages.${compiler}.callPackage ./nix/pretty-relative-time.nix { };
in
nixpkgs.haskell.packages.${compiler}.callPackage ./clusterview.nix { inherit data-prometheus pretty-relative-time; }
