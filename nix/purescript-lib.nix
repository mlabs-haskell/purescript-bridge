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

  localsDhall = pkgs.runCommand "locals-dhall"
    {
      inherit spagoLocalPkgs;
      preludeLocation = (pkgs.stdenv.mkDerivation {
        name = "dhall-prelude-location";
        version = "v15.0.0";
        src = pkgs.fetchurl {
          url = "https://prelude.dhall-lang.org/v15.0.0/Location/Type";
          sha256 = "uTa98yh//jqU/XcdUQ0RGHuZttABMDfL0FOyMhCxDss=";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      });
    }
    ''
      export LOCALS_DHALL=$out
      touch $LOCALS_DHALL
      cat <<DHALL > $LOCALS_DHALL
      let Location = $preludeLocation
      in
      DHALL
      for slp in $spagoLocalPkgs; do
        # FIXME: Want to use `dhall repl` but it does network IO
        slpName=$(grep name $slp/spago.dhall | cut -d "\"" -f 2)
        cat <<DHALL >> $LOCALS_DHALL
          {
            $slpName = Location.Local "$slp/spago.dhall"
          } // ($slp/spago.dhall).packages //
      DHALL
      done
      echo "{=}" >> $LOCALS_DHALL
    '';

  buildPursProject = { projectDir, pursSubDirs ? [ "/src" ], checkWarnings ? true }:
    pkgs.stdenv.mkDerivation rec {
      name = "purescript-lib-build-purs-project";
      outputs = [ "out" "spagoProjectDir" ];
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
        mkdir $spagoProjectDir
        cp -r ${projectDir}/* $spagoProjectDir
        ln -s ${localsDhall} $spagoProjectDir/locals.dhall

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

  runPursTest = args@{ projectDir, pursSubDirs ? [ "/src" "/test" ], checkWarnings ? false }:
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
