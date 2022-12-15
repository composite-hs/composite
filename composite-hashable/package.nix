{ mkDerivation, base, composite-base, hashable, hpack, lib }:
mkDerivation {
  pname = "composite-hashable";
  version = "0.8.2.1";
  src = ./.;
  libraryHaskellDepends = [ base composite-base hashable ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/composite-hs/composite#readme";
  description = "Orphan hashable instances";
  license = lib.licenses.bsd3;
}
