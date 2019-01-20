fetch: self: super: {
  haskellPackages = super.haskellPackages.override (oldArgs: {
    overrides = super.lib.composeExtensions (oldArgs.overrides or (_: _: {})) (hself: hsuper: {
      classy-prelude-yesod = super.haskell.lib.dontHaddock (hsuper.classy-prelude-yesod);
      cms-utils = hsuper.callCabal2nix "cms-utils" ../. {};
    });
  });
}
