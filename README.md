# elm2nix (alpha/unstable)

Convert Elm project into Nix expressions.

## Installation

  $ stack install --nix

## Usage

  $ cd my-project
  $ ~/.local/bin/elm2nix init > default.nix
  $ ~/.local/bin/elm2nix convert > elm-srcs.nix
  $ nix-build

## FAQ

Q: Why are there no Nix expressions yet to install elm2nix?
A: Waiting on https://github.com/input-output-hk/stack2nix 0.2 release

Q: Why is mkDerivation inlined into `default.nix`?
A: As it's considered unstable, it's generated for now. Might change in the future.
