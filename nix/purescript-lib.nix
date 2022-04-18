{ pkgs
, spago
, spagoPkgs
, spagoLocalPkgs ? [ ]
, nodejs
, purs
, nodeModules
}:
rec {
  pursFilterSource = pursDirs: builtins.filterSource (path: type: type == "regular" && builtins.elem (builtins.baseNameOf path) pursDirs) ./.;

  buildPursProject = { projectDir, pursSubDirs ? [ /src /test ], checkWarnings ? true }:
    pkgs.stdenv.mkDerivation rec {
      name = "purescript-lib-build-purs-project";
      pursDirs = (builtins.map (sd: projectDir + sd) pursSubDirs);
      src = pursFilterSource pursDirs;
      inherit spagoLocalPkgs;
      buildInputs = with spagoPkgs; [
        installSpagoStyle
        buildFromNixStore
      ];
      nativeBuildInputs = [
        purs
        spago
        pkgs.jq
      ];
      phases = [ "buildPhase" "checkPhase" "installPhase" ];
      doCheck = checkWarnings;
      buildPhase = ''
        install-spago-style
        PURS_SOURCES=$(for pursDir in $pursDirs; do find $pursDir -name "*.purs"; done)
        SPL_PURS_SOURCES=$(for slp in $spagoLocalPkgs; do find $slp -name "*.purs"; done)
        build-from-store $PURS_SOURCES $SPL_PURS_SOURCES --json-errors | grep '\{\"' > errors.json || build-from-store $PURS_SOURCES $SPL_PURS_SOURCES
      '';
      checkPhase = ''
        touch my_errors.json
        for ps in $PURS_SOURCES; do
          echo "checking for $ps"
          jq "(.errors[], .warnings[]) | select(.filename == \"$ps\")" errors.json >> my_errors.json
        done
        if [ -s my_errors.json ]; then
          echo "Compilation finished with warnings/errors"
          cat my_errors.json
          exit 1
        fi
      '';
      installPhase = ''
        mkdir $out
        mv errors.json $out/
        mv output $out/
        mv .spago $out/
      '';
    };

  runPursTest = args:
    let
      pursProject = buildPursProject args;
    in
    pkgs.stdenv.mkDerivation {
      name = "purescript-lib-run-purs-test";
      src = pursFilterSource [ ];
      inherit pursProject nodeModules;
      doCheck = true;
      buildInputs = [ nodejs ];
      checkPhase = ''
        export NODE_PATH=${nodeModules}/lib/node_modules
        node -e 'require("${pursProject}/output/Test.Main").main()' > out.log
      '';
      installPhase = ''
        cp out.log $out
      '';
    };
  bundlePursProjectCommonJs = args:
    let
      pursProject = buildPursProject args;
    in
    pkgs.stdenv.mkDerivation {
      name = "purescript-lib-bundle-purs-project-common-js";
      src = pursFilterSource [ ];
      buildInputs = [ spago purs ];
      installPhase = ''
        ln -s ${pursProject}/output output
        spago  --global-cache skip bundle-module --no-install --no-build --to $out
      '';
    };
}
