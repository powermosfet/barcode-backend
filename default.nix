{ pkgs ? import <nixpkgs> { } }:

  pkgs.haskellPackages.callPackage ./barcode-backend.nix { }
