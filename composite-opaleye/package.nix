{ mkDerivation, base, bytestring, composite-base, hpack, hspec
, lens, lib, opaleye, postgresql-simple, product-profunctors
, profunctors, QuickCheck, split, template-haskell, text, vinyl
}:
mkDerivation {
  pname = "composite-opaleye";
  version = "0.8.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring composite-base lens opaleye postgresql-simple
    product-profunctors profunctors split template-haskell text vinyl
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring composite-base hspec lens opaleye postgresql-simple
    product-profunctors profunctors QuickCheck split template-haskell
    text vinyl
  ];
  prePatch = "hpack";
  homepage = "https://github.com/composite-hs/composite#readme";
  description = "Opaleye SQL for Vinyl records";
  license = lib.licenses.bsd3;
}
