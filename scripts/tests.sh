#!/usr/bin/env bash

set -e

NIXPKGS_COMMIT=$(nix eval --impure --raw --expr "(builtins.fromJSON (builtins.readFile ./nixpkgs-src.json)).rev")
export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/$NIXPKGS_COMMIT.tar.gz

PATH="$(nix-build . --no-out-link)/bin:$PATH"

MYTMPDIR=$(mktemp -d)
trap "rm -rf $MYTMPDIR" EXIT

git clone https://github.com/evancz/elm-todomvc.git $MYTMPDIR/elm-todomvc

checkfile() {
  if [ ! -e "$1" ]; then
    echo "file missing: $1"
    return 1
  fi
}

pushd $MYTMPDIR/elm-todomvc
  elm2nix init > default.nix
  elm2nix convert > elm-srcs.nix
  elm2nix snapshot
  nix-build
  checkfile ./result/Main.html
popd
