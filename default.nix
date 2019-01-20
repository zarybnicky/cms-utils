with import ./nix {};
haskellPackages.callCabal2nix "cms-utils" ./. {}
