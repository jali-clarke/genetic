{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlays = {
        haskell = haskellSelf: haskellSuper: {
          genetic = haskellSelf.callCabal2nix "genetic" ./. { };
        };

        default = final: prev: {
          haskellPackages = prev.haskellPackages.override { overrides = self.overlays.haskell; };
          haskell = prev.haskell // {
            packages =
              let
                addPackages = _: compilerPackages: compilerPackages.override {
                  overrides = self.overlays.haskell;
                };
              in
              builtins.mapAttrs addPackages prev.haskell.packages;
          };
        };
      };
    } // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };

        cabalWrapped = pkgs.writeShellScriptBin "cabal" ''
          ${pkgs.hpack}/bin/hpack && exec ${pkgs.cabal-install}/bin/cabal "$@"
        '';

        format-all = pkgs.writeShellScriptBin "format-all" ''
          shopt -s globstar
          ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt . && ${pkgs.ormolu}/bin/ormolu -i src/**/*.hs
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          inputsFrom = [ pkgs.haskellPackages.genetic.env ];
          packages = [
            cabalWrapped
            format-all
          ];
        };
      }
    );
}
