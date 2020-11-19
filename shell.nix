let
  nixpkgs = import (import ./nix/sources.nix).nixpkgs {};
  package = nixpkgs.callPackage (import ./.) {};
in
  nixpkgs.haskellPackages.shellFor {
    packages = _: [package];
    nativeBuildInputs = [
      nixpkgs.cabal-install
      nixpkgs.hlint
      nixpkgs.ghcid
      nixpkgs.haskellPackages.doctest
    ];
  }
