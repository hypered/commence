let

  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;

  # Overlays let us override certain packages at a central location.
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };
  nixpkgs = import sources.nixpkgs { };
  hp = nixpkgs-overlayed.haskellPackages;

  inherit (nixpkgs.lib.attrsets) getAttrFromPath;

  contents = import ./nix/contents.nix { inherit nixpkgs; };

  nix-tooling = with hp; [ (callCabal2nix "niv" sources.niv { }) ];

  haskell-tooling = with hp; [ cabal-install ghcid hlint hasktags ];

  system-tooling = with nixpkgs; [
    inotify-tools # needed for HotExe.sh (filesystem notifs.)
    psmisc # needed for HotExe.sh: (kill processes by port.)
  ];

in hp.shellFor {
  packages = p: map (contents.getPkg p) (builtins.attrNames contents.pkgList);
  buildInputs = nix-tooling ++ haskell-tooling ++ system-tooling;
}
