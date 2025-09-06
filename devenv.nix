{ pkgs, lib, config, inputs, ... }:

{
  languages.haskell.enable = true;

  git-hooks.hooks = {
    ormolu.enable = true;
  };
}
