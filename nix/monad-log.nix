self: super:
let

  lib = super.lib;

  inherit (import ./sources.nix) gitignore monad-log; 

  stmContainersOverrides = selfh: superh: {
    monad-log =
      selfh.callCabal2nix "monad-log" monad-log { };
  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) stmContainersOverrides;
  });
}
