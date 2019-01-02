# to update: $ nix-prefetch-url --unpack url
let
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
in import (builtins.fetchTarball {
   url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
   inherit (spec) sha256;
}) { config = {}; overlays = []; }
