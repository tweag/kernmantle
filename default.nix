{ withHoogle ? false }:

let
localSources = {
  rowar = ./.;
};

overlayHaskell = _:pkgs:
  let
    bifunctorsSource = pkgs.fetchFromGitHub {
      owner = "ekmett";  # Needed for Arrow* instances on Product
      repo = "bifunctors";
      rev = "acd1866f8ddad7991ae31d5c66740fc697fef92b";
      sha256 = "0yni4g2ywmj4iajzfw4dyv6jjs8h5n870pd1y7x9pahc75a441jg";
    };

    extendWithSourcePackages = self: _:
      pkgs.lib.mapAttrs (name: src:
                           self.callCabal2nixWithOptions name src "" {})
                        localSources;

    inherit (pkgs.haskell.lib) doJailbreak dontCheck;
  in {
  haskellPackages =
    (pkgs.haskellPackages.override {
      overrides = self: super: rec {
        bifunctors = self.callCabal2nix "bifunctors" bifunctorsSource {};
        # vinyl hasn't been updated for hspec >= 2.7:
        vinyl = dontCheck super.vinyl;
      };
      }).extend extendWithSourcePackages;
};

# Nixpkgs clone
pkgs = import ./nixpkgs.nix {
  config = {
    allowBroken = true; # for a few packages, such as streaming-conduit
  };

  overlays = [ overlayHaskell ];
};

localPkgs = builtins.mapAttrs (x: _: pkgs.haskellPackages.${x}) localSources;

# The final shell
shell = pkgs.haskellPackages.shellFor {
  packages = _: builtins.attrValues localPkgs;
  nativeBuildInputs = [pkgs.cabal-install];
  withHoogle = withHoogle;
  # Generates the cabal and cabal project file
  shellHook =
  ''
    hpack .
  '';
};

# this derivation contains the shell and all the porcupine package in
# direct access
in
{
  inherit shell;
} // localPkgs
