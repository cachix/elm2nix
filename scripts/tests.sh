#!/usr/bin/env bash
set -e

ELM2NIX=$(stack exec which elm2nix)

git clone https://github.com/evancz/elm-todomvc.git $MYTMPDIR/elm-todomvc

checkfile() {
  if [ ! -e "$1" ]; then
    echo "file missing: $1"
    return 1
  fi
}

pushd $MYTMPDIR/elm-todomvc
  $ELM2NIX init > default.nix
  $ELM2NIX convert > elm-srcs.nix
  $ELM2NIX snapshot
  nix-build
  checkfile ./result/Main.html
popd
