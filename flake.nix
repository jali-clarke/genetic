{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      eachSystem =
        f: inputs.nixpkgs.lib.genAttrs systems (system: f inputs.nixpkgs.legacyPackages.${system});
    in
    {
      formatter = eachSystem (
        pkgs:
        (inputs.treefmt-nix.lib.evalModule pkgs {
          programs.nixfmt.enable = true;
          programs.ormolu.enable = true;
          programs.yamlfmt.enable = true;
        }).config.build.wrapper
      );

      overlays.haskell = hfinal: hprev: {
        genetic = hfinal.callCabal2nix "genetic" ./. { };
      };

      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          name = "genetic-dev";

          packages =
            let
              ghc = pkgs.haskell.packages.ghc984.ghcWithPackages (p: [
                p.hspec-expectations
                p.MonadRandom
                p.vector
                p.vector-algorithms
              ]);

              cabal-install = pkgs.writeShellScriptBin "cabal" ''
                ${pkgs.hpack}/bin/hpack --silent
                exec ${pkgs.cabal-install}/bin/cabal --active-repositories=:none "$@"
              '';
            in
            [
              cabal-install
              ghc
              pkgs.hpack
            ];
        };
      });
    };
}
