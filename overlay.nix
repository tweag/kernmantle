_: pkgs:

let
  # The cas-* packages are not in master yet.
  funflow-src = pkgs.fetchFromGitHub {
    owner  = "tweag";
    repo   = "funflow";
    rev    = "f10ee7ded42e4f4031d02958dbdbbd95f46791e3";
    sha256 = "12ndyhp6bb7cb7dda6jx6gwlp5yvg3h3y6m62n29608svkwrb0l5";
  };

	# The cabal2nix-generated default.nix is not appropriated, the repository has
  # a working one.
  hmatrix-sundials-src = pkgs.fetchFromGitHub {
    owner  = "haskell-numerics";
    repo   = "hmatrix-sundials";
    rev    = "9b6ec2b5fc509f74c5e61657dfc638a2c7ebced0";
    sha256 = "1djnzq28ahwh5yvfy2k7iqns496pr40nfqlrps5hhfl74l7qm88h";
  };

  # Version of profunctors with all instances (5.5.2).
  profunctors-src = pkgs.fetchFromGitHub {
    owner  = "ekmett";
    repo   = "profunctors";
    rev    = "8bc15927b3c6adc68ee229d7283c8cb17972e0ea";
    sha256 = "0pkcy755iby66533idxw78xrx6mdzci2j2avxjah9d3vxf43gn9r";
  };

  # HVega 0.5.0.0.
  hvega-src = pkgs.fetchFromGitHub {
    owner  = "DougBurke";
    repo   = "hvega";
    rev    = "bb5a0f46b7a7ee0214c1cac7dc6f3a93c3408527";
    sha256 = "1rh42psazayxlvzhsbgc98lbxwwy7cvgb0rjkbvyv29mcd62jsvs";
  };

  overrides = _: hspkgs:
    let
      call = name: hspkgs.callCabal2nix name "${funflow-src}/${name}";
      dontCheck = pkgs.haskell.lib.dontCheck;
    in
      {
        # Packages not in Hackage snapshot.
        cas-store = call "cas-store" {};
        cas-hashable = call "cas-hashable" {};
        profunctors = hspkgs.callCabal2nix "profunctors" profunctors-src {};
        hvega = hspkgs.callCabal2nix "hvega" "${hvega-src}/hvega" {};

        # Marked as broken.
        chell = hspkgs.callHackage "chell" "0.4.0.2" {};
        patience = hspkgs.callHackage "patience" "0.1.1" {};
        vinyl = dontCheck (hspkgs.callHackage "vinyl" "0.11.0" {});
        hmatrix-sundials = hspkgs.callPackage hmatrix-sundials-src {};

        # Tests failing.
        Diff = dontCheck hspkgs.Diff;
      };
in

{
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions
        (old.overrides or (_: _: {}))
        overrides;
  });
}
