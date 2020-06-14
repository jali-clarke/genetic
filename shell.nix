let
    # nixPkgs = import <nixpkgs> {};
    nixPkgs = import ../../nixpkgs {};
    stdenv = nixPkgs.stdenv;

    sys-packages = with nixPkgs; [
    ];

    tools = with nixPkgs; [
        cabal2nix
        cabal-install
        ghc
    ];
in stdenv.mkDerivation {
    name = "genetic";
    buildInputs = sys-packages ++ tools;

    shellHook = "
        alias update='cabal2nix . > genetic.nix'
        alias build='cabal build'
    ";
}
