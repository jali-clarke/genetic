{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlays.default = final: prev:
        let
          overrides = haskellSelf: haskellSuper: {
            genetic = haskellSelf.callCabal2nix "genetic" ./. { };
          };
        in
        {
          haskellPackages = prev.haskellPackages.override { inherit overrides; };
          haskell = prev.haskell // {
            packages = builtins.mapAttrs (_: compilerPackages: compilerPackages.override { inherit overrides; }) prev.haskell.packages;
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