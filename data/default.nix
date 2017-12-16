{ nixpkgs ? <nixpkgs>
, config ? {} }:

with (import nixpkgs config);

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , exactDependencies ? ./elm-stuff/exact-dependencies.json
    , src
    , name
    }:
    let
      sources = import srcs { inherit fetchzip; };
    in  stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ];

      preConfigurePhases = ["setupElmStuffPhase"];

      setupElmStuffPhase = ''
        runHook preSetupElmStuffPhase

        rm -rf elm-stuff
        mkdir elm-stuff
        ln -s \${exactDependencies} elm-stuff/exact-dependencies.json

        \${lib.concatStrings (lib.mapAttrsToList (name: src: ''
          mkdir -p elm-stuff/packages/\${name}
          ln -s \${src} elm-stuff/packages/\${name}/\${src.meta.version}
        '') sources)}

        runHook postSetupElmStuffPhase
      '';

      buildPhase = ''
        mkdir -p $out/share/doc
        elm make --warn --output $out/$name.js --docs $out/share/doc/$name.json
      '';

      installPhase = ":";
    };
in mkDerivation {
  name = "${name}";
  srcs = ./elm-srcs.nix;
  src = ./.;
}
