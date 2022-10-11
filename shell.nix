{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc922" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
