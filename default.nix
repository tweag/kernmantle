{ mkDerivation, aeson, base, bifunctors, bytestring, cas-hashable
, cas-store, comonad, containers, deepseq, directory, filepath
, hmatrix, hmatrix-csv, hmatrix-sundials, hpack, hvega, lens
, lens-aeson, optparse-applicative, path, path-io, profunctors
, safe-exceptions, stdenv, store, text, transformers
, unordered-containers, vector, vinyl
}:
mkDerivation {
  pname = "kernmantle";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors comonad containers lens profunctors safe-exceptions
    store transformers vinyl
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bifunctors bytestring cas-hashable cas-store comonad
    containers deepseq directory filepath hmatrix hmatrix-csv
    hmatrix-sundials hvega lens lens-aeson optparse-applicative path
    path-io profunctors safe-exceptions store text transformers
    unordered-containers vector vinyl
  ];
  prePatch = "hpack";
  homepage = "https://github.com/YPares/kernmantle#readme";
  description = "Composing Applicatives, Monads, Comonads and Arrows into Arrows";
  license = stdenv.lib.licenses.bsd3;
}
