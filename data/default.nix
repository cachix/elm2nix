{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? ["Main"]
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        versionsDat = ./versions.dat;
      };

      installPhase = let
        elmfile = module: "\${srcdir}/\${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p \$out/share/doc
        \${lib.concatStrings (map (module: ''
          elm make \${elmfile module} \
            --output \$out/\${module}.html \
            --docs \$out/share/doc/\${module}.json
        '') targets)}
      '';
    };
in mkDerivation {
  name = "${name}";
  srcs = ./elm-srcs.nix;
  src = ./.;
  srcdir = "${srcdir}";
  targets = [];
}
