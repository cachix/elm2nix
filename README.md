# elm2nix (alpha/unstable)

[![Build Status](https://travis-ci.org/domenkozar/elm2nix.svg?branch=master)](https://travis-ci.org/domenkozar/elm2nix)

Convert [Elm](http://elm-lang.org/) project into [Nix](https://nixos.org/nix/) expressions.

## Installation

    $ nix-shell -p stack --run "stack install --nix"

## Usage

    $ git clone https://github.com/w0rm/elm-flatris.git
    $ cd elm-flatris
    $ ~/.local/bin/elm2nix init > default.nix
    $ ~/.local/bin/elm2nix convert > elm-srcs.nix
    $ nix-build
    $ chromium ./result/index.html

## How it works

Given an Elm project with an `elm-package.json` generate:

1) a Nix expression in `default.nix` 
2) in addition to a helper file `elm-srcs.nix`
3) and the usual `elm-stuff/exact-dependencies.json`

If you run `nix-build` on generated expressions, you'll get
compiled html/js file and documentation json as an output of `elm make`.


## Running tests (as per travis)

    $ ./scripts/tests.sh

## FAQ

### Why are there no Nix expressions yet to install elm2nix?

Waiting on https://github.com/input-output-hk/stack2nix 0.2 release

### Why is mkDerivation inlined into `default.nix`?
As it's considered unstable, it's generated for now. Might change in the future.
