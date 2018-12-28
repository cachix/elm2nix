#!/usr/bin/env nix-shell
#!nix-shell -p git stack -i bash -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/release-18.09.tar.gz
export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/release-18.09.tar.gz

set -e

stack install --nix --pedantic

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
  ~/.local/bin/elm2nix init > default.nix
  ~/.local/bin/elm2nix convert > elm-srcs.nix
  nix-build
  checkfile ./result/Main.html
popd
