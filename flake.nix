{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          buildTools = hp:
            let
              # Workaround for https://github.com/NixOS/nixpkgs/issues/140774
              fixCyclicReference = drv:
                pkgs.haskell.lib.overrideCabal drv (_: {
                  enableSeparateBinOutput = false;
                });
            in
            {
              ghcid = fixCyclicReference hp.ghcid;
              haskell-language-server = hp.haskell-language-server.overrideScope
                (lself: lsuper: {
                  ormolu = fixCyclicReference hp.ormolu;
                });

              pandoc = pkgs.pandoc;
              texlive = pkgs.texlive.combined.scheme-full;
            };
        };
        packages.default = self'.packages.generics-sop-examples;
      };
    };
}
