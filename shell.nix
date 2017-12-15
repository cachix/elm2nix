with (import <nixpkgs> {});

let
  hsPkgs = haskell.packages.ghc802;
in haskell.lib.buildStackProject {
   name = "elm2nix";
   ghc = hsPkgs.ghc;
   buildInputs = [
     zlib hsPkgs.happy
   ];
}
