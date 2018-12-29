# to update: $ nix-prefetch-url --unpack url
import (builtins.fetchTarball {
   url = "https://github.com/NixOS/nixpkgs/archive/f55c7cfa9402bc0ec3d3f0d88ca1290e613dbfc2.tar.gz";
   sha256 = "0pd6l5mk5l6s3pfq64nzk204azp4642hddmfyn7h6g4ipsfmcxir";
}) { config = {}; overlays = []; }
