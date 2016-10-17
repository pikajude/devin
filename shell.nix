{ compiler ? "ghc801" }: (import <nixpkgs> {}).callPackage ./. { inherit compiler; }
