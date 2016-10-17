{ lib, haskell, writeTextDir, nodePackages, sass
, compiler ? "ghc801" }:

let
  pkg = haskell.packages."${compiler}".callPackage ./pkg.nix {};

  inShell = lib.inNixShell;

  releasePkg = haskell.lib.overrideCabal pkg (drv: rec {
    doHaddock = false;
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    isLibrary = false;
  });

in if inShell then releasePkg.env else releasePkg
