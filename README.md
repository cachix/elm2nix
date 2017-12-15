# elm2nix (alpha/unstable)

[![Build Status](https://travis-ci.org/domenkozar/elm2nix.svg?branch=master)](https://travis-ci.org/domenkozar/elm2nix)

Convert Elm project into Nix expressions.

## Installation

    $ stack install --nix

## Usage

    $ cd my-project
    $ ~/.local/bin/elm2nix init > default.nix
    $ ~/.local/bin/elm2nix convert > elm-srcs.nix
    $ nix-build

## Running tests (as per travis)

    $ ./scripts/tests.sh

## FAQ

### Why are there no Nix expressions yet to install elm2nix?

Waiting on https://github.com/input-output-hk/stack2nix 0.2 release

### Why is mkDerivation inlined into `default.nix`?
As it's considered unstable, it's generated for now. Might change in the future.
