# elm2nix

![Build Status](https://github.com/cachix/elm2nix/workflows/Test/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/elm2nix.svg)](https://hackage.haskell.org/package/elm2nix)

Convert an [Elm](http://elm-lang.org/) project into
[Nix](https://nixos.org/nix/) expressions.

It consists of multiple commands:
- `elm2nix convert`: Given `elm.json` in current directory, all dependencies are
  parsed and their sha256sum calculated
- `elm2nix snapshot`: Reads packages from `elm.json` and writes them to binary cache file `registry.dat` used by [elm-compiler](https://github.com/elm/compiler/blob/047d5026fe6547c842db65f7196fed3f0b4743ee/builder/src/Stuff.hs#L147).
- `elm2nix init`: Generates `default.nix` that glues everything together

## Assumptions

Supports Elm 0.19.1

## Installation

### From nixpkgs (recommended)

Make sure you have up to date stable or unstable nixpkgs channel.

    $ nix-env -i elm2nix

### From source

    $ git clone https://github.com/domenkozar/elm2nix.git
    $ cd elm2nix
    $ nix-env -if .

## Usage

    $ git clone https://github.com/evancz/elm-todomvc.git
    $ cd elm-todomvc
    $ elm2nix init > default.nix
    $ elm2nix convert > elm-srcs.nix
    # generates ./registry.dat
    $ elm2nix snapshot
    $ nix-build
    $ chromium ./result/Main.html

## Running tests (as per CI)

    $ ./scripts/tests.sh

## FAQ

### Why is mkDerivation inlined into `default.nix`?

As it's considered experimental, it's generated for now. Might change in the future.

### How do I use elm2nix with [ParcelJS][parceljs] and [Yarn][yarn-v1]?

Instead of running `elm2nix init`, create a `default.nix` with the following derivation:

```nix
{ pkgs ? import <nixpkgs> {}
}:

let
  yarnPkg = pkgs.mkYarnPackage {
    name = "myproject-node-packages";
    src = pkgs.lib.cleanSourceWith {
      src = ./.;
      name = "myproject-node-packages.json";
      filter = name: type: baseNameOf (toString name) == "package.json";
    };
    yarnLock = ./yarn.lock;
    publishBinsFor = ["parcel"];
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
    elmVersion = "0.19.1";
    registryDat = ./registry.dat;
  };

  installPhase = ''
    mkdir -p $out
    parcel build --dist-dir $out index.html
  '';
}
```

[parceljs]: https://parceljs.org/
[yarn-v1]: https://classic.yarnpkg.com/lang/en/
