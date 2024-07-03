# Does not pin Nixpkgs. Do that on your own if you feel like it.
let
  pkgs = import <nixpkgs> {};
in
pkgs.stdenv.mkDerivation {
  name = "AP2024";
  buildInputs =
    with pkgs;
    [
      haskell.compiler.ghc96
      cabal-install
    ];
}
