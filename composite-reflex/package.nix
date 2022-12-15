{ mkDerivation, base, composite-base, data-default, dependent-map
, dependent-sum, hpack, http-api-data, lens, lib, reflex
, reflex-dom, text, vinyl
}:
mkDerivation {
  pname = "composite-reflex";
  version = "0.8.2.1";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base data-default dependent-map dependent-sum
    http-api-data lens reflex reflex-dom text vinyl
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/composite-hs/composite#readme";
  description = "Utilities for using composite records and corecords with Reflex";
  license = lib.licenses.bsd3;
}
