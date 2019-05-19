{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

let
  mkDerivation =
    { elmSrcs ? ./elm-srcs.nix
    , srcs
    , srcDir ? "./src"
    , extension ? ".elm"
    , name
    , targets ? []
    , versionsDat ? ./versions.dat
    }:
    stdenv.mkDerivation {
      inherit name srcs;
      sourceRoot = ".";

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import elmSrcs;
        inherit versionsDat;
      };

      installPhase = let
        elmfile = module: "\${srcDir}/\${builtins.replaceStrings ["."] ["/"] module}\${extension}";
        elmJson = pkgs.lib.importJSON ./elm.json;
        elmJsonFile = pkgs.writeText "elm.json" (builtins.toJSON generatedElmJson);
        genSrcs = xs: map (path: if path == "." then srcDir else builtins.replaceStrings [".." "/"] ["" ""] path) xs;
        generatedElmJson = with pkgs.lib;
          if hasAttrByPath ["source-directories"] elmJson then
            attrsets.mapAttrs
              (name: value: if name == "source-directories" then genSrcs value else value)
              elmJson
          else
            elmJson;
      in ''
        mkdir -p \$out/share/doc

        cp \${elmJsonFile} ./elm.json
        echo "Generating new elm.json..."
        cat elm.json

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
  srcDir = "${srcDir}";
}
