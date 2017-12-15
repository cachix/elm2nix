#!/usr/bin/env nix-shell
#!nix-shell -p git stack -i bash

stack install --nix

MYTMPDIR=$(mktemp -d)
trap "rm -rf $MYTMPDIR" EXIT

git clone https://github.com/debois/elm-mdl.git $MYTMPDIR/elm-mdl

pushd $MYTMPDIR/elm-mdl
  ~/.local/bin/elm2nix init > default.nix
  ~/.local/bin/elm2nix convert > elm-srcs.nix
  nix-build
popd
