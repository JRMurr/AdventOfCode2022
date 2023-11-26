{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    roc.url = "github:roc-lang/roc";
    roc2nix = {
      url = "github:JRMurr/roc2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.roc.follows = "roc";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, roc, roc2nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        rocPkgs = roc.packages.${system};
        rocLib = (roc2nix.lib.${system}).overrideToolchain rocPkgs.cli;
        # aoc-cli = pkgs.callPackage ./aoc-cli { };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # haskell.compiler.ghc94
            # (haskell-language-server.override {
            #   supportedGhcVersions = [ "942" ];
            # })
            ghc
            haskell-language-server
            ormolu
            cabal-install
            just

            rocPkgs.cli
            rocPkgs.lang-server

            aoc-cli
          ];
        };
        # packages = { aoc-cli = aoc-cli; };
      });
}
