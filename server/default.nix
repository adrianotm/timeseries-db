{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  server = pkgs.haskellPackages.callCabal2nix "server" ./. {
    common = pkgs.haskellPackages.callCabal2nix "common" ../common {};
  };

in

  if pkgs.lib.inNixShell then server.env else server
