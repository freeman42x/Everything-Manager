{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, generic-lens, hedgehog
      , lens, lib, miso, opaleye, postgresql-simple, product-profunctors
      , text
      }:
      mkDerivation {
        pname = "Everything-Manager";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers generic-lens hedgehog lens miso opaleye
          postgresql-simple product-profunctors text
        ];
        executableHaskellDepends = [
          base containers generic-lens hedgehog lens miso opaleye
          postgresql-simple product-profunctors text
        ];
        testHaskellDepends = [
          base containers generic-lens hedgehog lens miso opaleye
          postgresql-simple product-profunctors text
        ];
        doHaddock = false;
        description = "System used to optimize the efficiency of achieving goals";
        license = lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
