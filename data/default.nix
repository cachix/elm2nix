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
        # TODO: review naming
        elmJson = pkgs.lib.importJSON ./elm.json;
        elmJsonFile = pkgs.writeText "elm.json" (builtins.toJSON generatedElmJson);
        genSrcs = xs: map (path: if path == "." then srcDir else builtins.replaceStrings [".." "/"] ["" ""] path) xs;
        generatedElmJson = with pkgs.lib;
          if hasAttrByPath ["source-directories"] elmJson then
            # TODO: check if there isn't better function
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
  # TODO: given we need to process elm.json via nix anyway
  # it might be better to just read this from elm.json?
  #   - check if we're possible to get to elm.json
  #   - consult with maitainer
  srcs = ${srcs};
  targets = ["Main"];
  srcDir = "${srcDir}";
}
