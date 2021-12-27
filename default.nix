{ mkDerivation, array, base, bimap, bitwise, containers, extra
, HUnit, lens, lib, linear, mod, mtl, parsec, QuickCheck, random
, relude, safe, template-haskell, text, vector
}:
mkDerivation {
  pname = "crossbow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bimap bitwise containers extra lens linear mod mtl
    parsec random relude safe template-haskell text vector
  ];
  executableHaskellDepends = [
    array base bimap bitwise containers extra lens linear mod mtl
    parsec random relude safe template-haskell text vector
  ];
  testHaskellDepends = [
    array base bimap bitwise containers extra HUnit lens linear mod mtl
    parsec QuickCheck random relude safe template-haskell text vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
