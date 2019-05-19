{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

let
  mkDerivation =
    { elmSrcs ? ./elm-srcs.nix
    , srcs
    , sourceRoot ? "./src"
    , name
    , targets ? []
    , versionsDat ? ./versions.dat
    }:
    stdenv.mkDerivation {
      inherit name srcs sourceRoot;

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import elmSrcs;
        inherit versionsDat;
      };

      installPhase = let
        elmfile = module: "\${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p \$out/share/doc
        \${lib.concatStrings (map (module: ''
          echo "compiling \${elmfile module}"
          elm make \${elmfile module} --output \$out/\${module}.html --docs \$out/share/doc/\${module}.json
        '') targets)}
      '';
    };
in mkDerivation {
  name = "${name}";
  elmSrcs = ./elm-srcs.nix;
  # TODO: add haskell paths
  srcs = ${srcs};
  targets = ["Main"];
  sourceRoot = "${sourceRoot}";
}
