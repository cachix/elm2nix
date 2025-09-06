#!/usr/bin/env bash
set -e

WORKDIR=$(mktemp -d)
trap "rm -rf $WORKDIR" EXIT

ELM2NIX=$(cabal exec which elm2nix)

git clone https://github.com/evancz/elm-todomvc.git $WORKDIR/elm-todomvc

checkfile() {
  if [ ! -e "$1" ]; then
    echo "file missing: $1"
    return 1
  fi
}

pushd $WORKDIR/elm-todomvc
  $ELM2NIX init > default.nix
  $ELM2NIX convert > elm-srcs.nix
  $ELM2NIX snapshot
  nix-build
  checkfile ./result/Main.html
popd
