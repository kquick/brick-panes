{
  description = "Flake to build the haskell-src package 'brick-panes'";

  nixConfig.bash-prompt-suffix = "brick-panes.env} ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, levers, nixpkgs }:
    rec {

      devShells = levers.haskellShells
        { inherit nixpkgs;
          flake = self;
          defaultPkg = "brick-panes";
          ghcvers = s: ["ghc912"];
          # additionalPackages = pkgs.haskell.packages.ghc8107.profiteur
        };

      packages = levers.eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
          mkHaskell = levers.mkHaskellPkg { inherit nixpkgs system; };
        in rec {
          default = brick-panes;
          brick-panes = mkHaskell "brick-panes" self {};
        });
    };
}
