{ nixpkgs ?
     (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      rev = "c29d2fde74d03178ed42655de6dee389f2b7d37f";
      sha256 = "1v1cnlhqp6lcjbsakyiaqk2mm21gdj74d1i7g75in02ykk5xnc7k";
     }) {})
, compiler ? "ghc802" # TODO: try using "default"?
, haskellPackages ? if compiler == "default"
                      then nixpkgs.pkgs.haskellPackages
                      else nixpkgs.pkgs.haskell.packages.${compiler}
}:

with nixpkgs.pkgs.haskell.lib;

let

  inherit (nixpkgs) pkgs;

  cleanSrc =
    builtins.filterSource (path: type:
      nixpkgs.lib.all (i: toString i != path) [ ./default.nix ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" "result" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i path)) [ ".ghc.environment." ]
      ) ./.;

  extendedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      elm-package = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "elm-package" (pkgs.fetchFromGitHub {
        owner = "elm-lang"; repo = "elm-package";
        rev = "2a5d2de0b55d4c9a30bec71f1cc6ff80130d7dfe";
        sha256 = "18v3qf2i125bf4h91smmcm79l23pjrz6ym5q06sxifdarzz5jd4n";
      }) {});
      elm-compiler = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "elm-compiler" (pkgs.fetchFromGitHub {
        owner = "elm-lang"; repo = "elm-compiler";
        rev = "7ee7742a16188df7ff498ec4ef9f8b49e58a35fe";
        sha256 = "0imagr4v0455sr04ya0sksd3358xqgx189c67xg5q8aj88hr9ym3";
      }) {}));

      http-types = self.callHackage "http-types" "0.8.6" {};
      http-client = self.callHackage "http-client" "0.4.31.2" {};
      http-client-tls = self.callHackage "http-client-tls" "0.2.4.1" {};
      aeson-pretty = super.aeson-pretty_0_7_2;

      elm2nix = pkgs.haskell.lib.justStaticExecutables (self.callCabal2nix "elm2nix" cleanSrc {});
    };
  };

  drv = extendedHaskellPackages.elm2nix;

in if pkgs.lib.inNixShell then drv.env else drv

