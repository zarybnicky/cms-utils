with import <nixpkgs> {};

(haskellPackages.override (old: {
  overrides = lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
    classy-prelude-yesod = haskell.lib.dontHaddock (super.classy-prelude-yesod);
    cms-utils = super.callCabal2nix "cms-utils" ./. {};
  });
})).cms-utils
