let
  nixpkgs = import (import ./nix/sources.nix).nixpkgs {};
  package = nixpkgs.callPackage (import ./.) {};
in
  nixpkgs.haskellPackages.shellFor {
    packages = _: [package];
    nativeBuildInputs = [
      nixpkgs.cabal-install
      nixpkgs.entr
      nixpkgs.ghcid
      nixpkgs.haskellPackages.doctest
      nixpkgs.hlint
    ];
  }
