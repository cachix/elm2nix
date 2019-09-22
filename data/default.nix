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
    , targets ? []
    , versionsDat ? ./versions.dat
    , outputJavaScript ? false
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        inherit versionsDat;
      };

      installPhase = let
        elmfile = module: "\${srcdir}/\${builtins.replaceStrings ["."] ["/"] module}.elm";
        extension = if outputJavaScript then "js" else "html";
      in ''
        mkdir -p \$out/share/doc
        \${lib.concatStrings (map (module: ''
          echo "compiling \${elmfile module}"
          elm make \${elmfile module} --output \$out/\${module}.\${extension} --docs \$out/share/doc/\${module}.json
        '') targets)}
      '';
    };
in mkDerivation {
  name = "${name}";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = ["Main"];
  srcdir = "${srcdir}";
  outputJavaScript = false;
}
