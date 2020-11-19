{ haskellPackages }:
  haskellPackages.callCabal2nix "course" ./. {}
