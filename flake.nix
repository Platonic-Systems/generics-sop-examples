{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
          ];
          devShell.tools = hp: {
            inherit (hp) cabal-fmt;
          };
        };
        packages.default = self'.packages.generics-sop-examples;
      };
    };
}
