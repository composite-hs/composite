{ mkDerivation, base, binary, composite-base, hpack, lib }:
mkDerivation {
  pname = "composite-binary";
  version = "0.8.1.0";
  src = ./.;
  libraryHaskellDepends = [ base binary composite-base ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/composite-hs/composite#readme";
  description = "Orphan binary instances";
  license = lib.licenses.bsd3;
}
