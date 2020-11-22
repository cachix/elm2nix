# elm2nix

![Build Status](https://github.com/cachix/elm2nix/workflows/Test/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/elm2nix.svg)](https://hackage.haskell.org/package/elm2nix)

Convert an [Elm](http://elm-lang.org/) project into
[Nix](https://nixos.org/nix/) expressions.

It consists of multiple commands:
- `elm2nix convert`: Given `elm.json` in current directory, all dependencies are
  parsed and their sha256sum calculated
- `elm2nix snapshot`: Downloads snapshot of http://package.elm-lang.org into `versions.dat`
- `elm2nix init`: Generates `default.nix` that glues everything together

## Assumptions

Supports Elm 0.19.x

## Installation

### From nixpkgs (recommended)

Make sure you have up to date stable or unstable nixpkgs channel.

    $ nix-env -iA elm2nix

### From source

    $ git clone https://github.com/domenkozar/elm2nix.git
    $ cd elm2nix
    $ nix-env -if .

## Usage

    $ git clone https://github.com/evancz/elm-todomvc.git
    $ cd elm-todomvc
    $ elm2nix init > default.nix
    $ elm2nix convert > elm-srcs.nix
    $ elm2nix snapshot > versions.dat
    $ nix-build
    $ chromium ./result/Main.html

## Running tests (as per CI)

    $ ./scripts/tests.sh

## FAQ

### Why is mkDerivation inlined into `default.nix`?

As it's considered experimental, it's generated for now. Might change in the future.

### How to use with ParcelJS and Yarn?

Instead of running `elm2nix init`, use something like:

```nix
{ pkgs ? import <nixpkgs> {}
}:

let
  yarnPkg = pkgs.yarn2nix.mkYarnPackage {
    name = "myproject-node-packages";
    packageJSON = ./package.json;
    unpackPhase = ":";
    src = null;
    yarnLock = ./yarn.lock;
    publishBinsFor = ["parcel-bundler"];
  };
in pkgs.stdenv.mkDerivation {
  name = "myproject-frontend";
  src = pkgs.lib.cleanSource ./.;

  buildInputs = with pkgs.elmPackages; [
    elm
    elm-format
    yarnPkg
    pkgs.yarn
  ];

  patchPhase = ''
    rm -rf elm-stuff
    ln -sf ${yarnPkg}/node_modules .
  '';

  shellHook = ''
    ln -fs ${yarnPkg}/node_modules .
  '';

  configurePhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./elm-srcs.nix;
    versionsDat = ./versions.dat;
  };

  installPhase = ''
    mkdir -p $out
    parcel build -d $out index.html
  '';
}
```
