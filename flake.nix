{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        aoc-cli = pkgs.callPackage ./aoc-cli { };
      in {
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

            aoc-cli
          ];
        };
        packages = { aoc-cli = aoc-cli; };
      });
}
