# AOC2022

nix build cache is https://app.cachix.org/cache/jrmurr-aoc2022#pull

run
```sh
cachix use jrmurr-aoc2022

# push
nix develop --profile dev-profile # may need to exit shell
cachix push jrmurr-aoc2022 dev-profile
```