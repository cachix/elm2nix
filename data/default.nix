{ nixpkgs ? <nixpkgs>
, config ? {}
}:
with (import nixpkgs config);
let
  mkDerivation =
    { elmSrcs ? ./elm-srcs.nix
    , srcs
    , srcdir ? "${srcdir}"
    , name
    , targets ? []
    , versionsDat ? ./versions.dat
    }:
    let sanitizePath = str: lib.concatStringsSep "/"
        (map (p: if p == "." then srcdir else p)
          (builtins.filter (p: p != "..")
            (lib.splitString "/" str)));
    in stdenv.mkDerivation {
      inherit name srcs;
      sourceRoot = ".";

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import elmSrcs;
        inherit versionsDat;
      };

      patchPhase = let
        elmJsonFile = with lib;
          let elmjson = importJSON ./elm.json;
          in writeText "elm.json" (builtins.toJSON
            (if hasAttrByPath ["source-directories"] elmjson then
              elmjson // { source-directories = (map sanitizePath elmjson.source-directories); }
            else
              elmjson
          ));
      in ''
        cp \${elmJsonFile} ./elm.json
        echo "Generating new elm.json..."
        cat elm.json
      '';

      installPhase = ''
        mkdir -p $out/share/doc

        \${lib.concatStrings (map (module:
          let fullmodule = sanitizePath module;
              modulename = sanitizePath (lib.removePrefix "./" module);
          in ''
            echo "compiling \${module}"
            elm make \${fullmodule}.elm --output $out/\${modulename}.html --docs $out/share/doc/\${modulename}.json
          '') targets)}
      '';
    };
in mkDerivation {
  name = "${name}";
  # TODO: given we need to process elm.json via nix anyway
  # it might be better to just read this from elm.json?
  #   - check if we're possible to get to elm.json
  #   - consult with maitainer
  srcs = ${srcs};
  targets = ["./Main"];
}
