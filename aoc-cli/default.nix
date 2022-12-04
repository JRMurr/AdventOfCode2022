{ pkgs }:
# https://github.com/scarvalhojr/aoc-cli/tree/main
pkgs.rustPlatform.buildRustPackage rec {
  version = "0.6.0";
  pname = "aoc-cli";

  src = pkgs.fetchFromGitHub {
    owner = "scarvalhojr";
    repo = pname;
    rev = "${version}";
    sha256 = "sha256-aDbW//DmJiQO0/RJ1JnxAm4ImsfO6TYajmlO43CruWo=";
  };
  cargoSha256 = "sha256-mcZdFWHXnj8bw6VV2Zjzai175q/GYXHwgmcUjNv8vvE";

  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.openssl ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
    [ pkgs.darwin.apple_sdk.frameworks.Security ];
}
