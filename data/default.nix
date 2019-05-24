{ nixpkgs ? <nixpkgs>
, config ? {}
}:
with (import nixpkgs config);
let
  mkDerivation =
    { elmsrcs ? ./elm-srcs.nix
    , srcs
    , srcdir ? "src"
    , name
    , targets ? []
    , versionsDat ? ./versions.dat
    }:
    let sanitizePath = str:
        let path = builtins.filter (p: p != "..") (lib.splitString "/" str);
        in lib.concatStringsSep "/" (map (p: if p == "." then srcdir else p) path);
    in stdenv.mkDerivation {
      inherit name srcs;
      sourceRoot = ".";

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import elmsrcs;
        inherit versionsDat;
      };

      patchPhase = let
        elmjson = let json = (lib.importJSON ./elm.json) // { source-directories = map sanitizePath srcs; };
        in writeText "elm.json" (builtins.toJSON json);
      in ''
        echo "Generating elm.json"
        cp \${elmjson} ./elm.json
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
  srcs = ${srcs};
  srcdir = "${srcdir}";
  targets = ["./Main"];
}
