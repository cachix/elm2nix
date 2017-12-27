#!/usr/bin/env nix-shell
#!nix-shell -p git stack -i bash

set -e

stack install --nix

MYTMPDIR=$(mktemp -d)
trap "rm -rf $MYTMPDIR" EXIT

git clone https://github.com/debois/elm-mdl.git $MYTMPDIR/elm-mdl

checkfile() {
  if [ ! -e "$1" ]; then
    echo "file missing: $1"
    return 1
  fi
}

pushd $MYTMPDIR/elm-mdl
  ~/.local/bin/elm2nix init | sed -e '/^  targets/s/\[\]/["Material" "Material.Button"]/' > default.nix
  ~/.local/bin/elm2nix convert > elm-srcs.nix
  nix-build
  checkfile ./result/Material.js
  checkfile ./result/Material.Button.js
popd
