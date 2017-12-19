{ nixpkgs ? <nixpkgs>
, config ? {} }:

with (import nixpkgs config);

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    }:
    let
      sources = import srcs { inherit fetchzip; };
      exactDependencies = builtins.toFile "exact-dependencies.json"
        (builtins.toJSON (lib.mapAttrs (name: value: value.version) sources));
    in stdenv.mkDerivation {
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
          ln -s \${src.src} elm-stuff/packages/\${name}/\${src.version}
        '') sources)}

        runHook postSetupElmStuffPhase
      '';

      buildPhase = let
        elmfile = module: "\${srcdir}/\${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p \$out/share/doc
        \${lib.concatStrings (map (module: ''
          elm make --warn \${elmfile module} --output \$out/\${module}.js --docs \$out/share/doc/\${module}.json
        '') targets)}
      '';

      installPhase = ":";
    };
in mkDerivation {
  name = "${name}";
  srcs = ./elm-srcs.nix;
  src = ./.;
  srcdir = "${srcdir}";
  targets = [];
}
