let
  import-json-spec = import ./import-json-spec.nix;
  iohk-overlay = import-json-spec ./haskell.nix-src.json;
in
{ pkgs ? import-json-spec ./nixpkgs-src.json iohk-overlay
}:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; };
  }
