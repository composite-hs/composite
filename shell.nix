{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc8107
    pkgs.zlib
    pkgs.postgresql
    pkgs.pkg-config
  ];
}
