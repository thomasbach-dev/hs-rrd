{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc922" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hs-rrd.nix { }
