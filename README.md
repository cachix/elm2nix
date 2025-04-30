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

It's already included in [devenv](https://devenv.sh/getting-started/) when using Elm:

```nix
{
  langauges.elm.enable = true;

  tasks = {
    "frontend:elm2nix" = {
      exec = "cd frontend && elm2nix convert > elm-srcs.nix && elm2nix snapshot";
      execIfModified = [ "frontend/elm.json" ];
      before = [ "devenv:enterShell" ];
    };
  };

}
```

## Running tests (as per CI)

    $ ./scripts/tests.sh

## FAQ

### Why is mkDerivation inlined into `default.nix`?

As it's considered experimental, it's generated for now. Might change in the future.

### How do I use elm2nix with [ParcelJS][parceljs] and [Yarn][yarn-v1]?

Instead of running `elm2nix init`, create a `default.nix` with the following derivation:

https://github.com/cachix/elm2nix/issues/49#issuecomment-1696082884

[parceljs]: https://parceljs.org/
[yarn-v1]: https://classic.yarnpkg.com/lang/en/
