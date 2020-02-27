with (import ./nixpkgs.nix {
  overlays = [(import ./overlay.nix)];
});

(haskellPackages.callPackage ./. {}).env
