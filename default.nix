let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
  contents = import ./nix/contents.nix { inherit nixpkgs; }; 
  packages = builtins.attrNames contents.pkgList; 
  inherit (nixpkgs.lib.attrsets) mapAttrs;

  # Generate outputs using the same list of package names everywhere. 
  mkOutput = name: _: nixpkgs.haskellPackages."${name}";

in mapAttrs mkOutput contents.pkgList
