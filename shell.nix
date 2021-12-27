{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bimap, bitwise, containers
      , extra, haskeline, HUnit, lens, lib, linear, mod, mtl, parsec
      , QuickCheck, random, relude, safe, template-haskell, text, vector
      }:
      mkDerivation {
        pname = "crossbow";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base bimap bitwise containers extra haskeline lens linear mod
          mtl parsec random relude safe template-haskell text vector
        ];
        executableHaskellDepends = [
          array base bimap bitwise containers extra haskeline lens linear mod
          mtl parsec random relude safe template-haskell text vector
        ];
        testHaskellDepends = [
          array base bimap bitwise containers extra haskeline HUnit lens
          linear mod mtl parsec QuickCheck random relude safe
          template-haskell text vector
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
