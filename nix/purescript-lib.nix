{ pkgs
, projectName
, spago
, spagoPkgs
, spagoLocalPkgs ? [ ]
, resourceDirs ? [ ]
, nodejs
, purs
, nodeModules
, mainModule ? "Main"
, testModule ? "Test.Main"
}:
rec {
  pursFilterSource = pursDirs: builtins.filterSource (path: type: type == "regular" && builtins.elem (builtins.baseNameOf path) pursDirs) ./.;

  localsDhall = pkgs.runCommand "${projectName}-locals-dhall"
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
      name = "${projectName}-build-purs-project";
      outputs = [ "out" "compilationOut" ];
      pursDirs = (builtins.map (sd: projectDir + sd) pursSubDirs);
      src = pursFilterSource pursDirs;
      inherit spagoLocalPkgs resourceDirs;
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
        set -vox
        mkdir $out
        for resDir in $resourceDirs; do
          cp -r $resDir $out/
        done
        cp -r ${projectDir}/* $out
        ln -s ${localsDhall} $out/locals.dhall

        install-spago-style
        PURS_SOURCES=$(for pursDir in $pursDirs; do find $pursDir -name "*.purs"; done)
        LOCALS_GLOBS=$(for slp in $spagoLocalPkgs; do find $slp/src -name "*.purs"; done)
        build-from-store $PURS_SOURCES $LOCALS_GLOBS --json-errors | grep '\{\"' > errors.json || build-from-store $PURS_SOURCES $LOCALS_GLOBS
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
        mkdir $compilationOut
        mv errors.json $compilationOut/
        mv output $compilationOut/
        mv .spago $compilationOut/
      '';
    };

  runWithNode = args@{ projectDir, pursSubDirs ? [ "/src" ], checkWarnings ? false }:
    let
      pursProject = buildPursProject args;
      makeBundle = { myScriptName, myScript, buildInputsAdditional ? [ ] }:
        pkgs.symlinkJoin rec {

          name = "${myScriptName}";

          script =
            (pkgs.writeScriptBin name myScript).overrideAttrs
              (old: {
                buildCommand = ''
                  ${old.buildCommand}
                  patchShebangs $out'';
              });

          paths = [ script ] ++ buildInputs;

          buildInputs = [ pkgs.makeWrapper ] ++ buildInputsAdditional;

          postBuild = ''
            wrapProgram $out/bin/${myScriptName} \
              --prefix PATH : $out/bin \
              --prefix PATH : $out/lib/node_modules/.bin
          '';

        };
    in
    makeBundle {
      myScriptName = "${projectName}-run-with-node";
      myScript = ''
        export NODE_PATH=${nodeModules}/lib/node_modules
        ${nodejs}/bin/node -e 'require("${pursProject.compilationOut}/output/${mainModule}").main()'
      '';
    };

  testWithNode = args@{ projectDir, pursSubDirs ? [ "/src" "/test" ], checkWarnings ? false }:
    let
      pursProject = buildPursProject args;
    in
    pkgs.stdenv.mkDerivation {
      name = "${projectName}-test-with-node";
      src = pursFilterSource [ ];
      inherit pursProject nodeModules;
      doCheck = true;
      buildInputs = [ nodejs ];
      checkPhase = ''
        export NODE_PATH=${nodeModules}/lib/node_modules
        cp -r ${pursProject.out} .
        node -e 'require("${pursProject.compilationOut}/output/${testModule}").main()' > out.log
      '';
      installPhase = ''
        cp out.log $out
      '';
    };

  bundleCommonJs = args:
    let
      pursProject = buildPursProject args;
    in
    pkgs.stdenv.mkDerivation {
      name = "${projectName}-bundle-common-js";
      src = pursFilterSource [ ];
      buildInputs = [ spago purs ];
      installPhase = ''
        ln -s ${pursProject.compilationOut}/output output
        spago  --global-cache skip bundle-module --no-install --no-build --main ${mainModule} --to $out
      '';
    };
}
