let
  # Pinning nixpkgs for reproducibility. If you want to use your
  # system Nixpkgs, use the other definition of 'nixpkgs' that is
  # commented out below.
  nixpkgs = builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz;
    sha256 = "sha256:1lr1h35prqkd1mkmzriwlpvxcb34kmhc9dnr48gkm8hh089hifmx";
  };
  # nixpkgs = <nixpkgs>;

  pkgs = import nixpkgs {};
in
pkgs.stdenv.mkDerivation {
  name = "AP2024";
  buildInputs =
    with pkgs;
    [
      haskell.compiler.ghc96
      cabal-install
      haskell-language-server
    ];
}
