{ pkgs, easy-ps }:
let
  makeBundle = { myScriptName, myScript, buildInputsAdditional ? [ ] }:
    pkgs.symlinkJoin rec {

      name = "${myScriptName}-bundle";

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
        wrapProgram $out/bin/${name} \
          --prefix PATH : $out/bin \
          --prefix PATH : $out/lib/node_modules/.bin
      '';

    };
in
{
  inherit makeBundle;

  checkers = pkgs:
    let
      nixpkgs-fmt = pkgs.nixpkgs-fmt;
      fourmolu = pkgs.haskellPackages.fourmolu;
      hlint = pkgs.hlint;
      cabal-fmt = pkgs.haskellPackages.cabal-fmt;
      shfmt = pkgs.shfmt;
      shellcheck = pkgs.shellcheck;
      dhall = pkgs.dhall;
      purs-tidy = easy-ps.purs-tidy;

    in
    {
      checkNixFiles = src: pkgs.runCommand "check-nix-files" { }
        ''
          touch $out
          echo "Check Nix files in ${src}"
          FILES=$(find ${src} -name "*.nix")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${nixpkgs-fmt}/bin/nixpkgs-fmt --check $FILES
        '';

      checkHaskellFiles = src: pkgs.runCommand "check-haskell-files" { }
        ''
          touch $out
          echo "Check Haskell files in ${src}"
          EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -XBangPatterns -o -fplugin=RecordDotPreprocessor"
          FILES=$(find ${src} -name "*.hs")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${fourmolu}/bin/fourmolu --mode check --check-idempotence $EXTENSIONS $FILES
          if [ $? -ne 0 ]; then
            echo "Fourmolu complained."
            exit 1
          fi
          EXTENSIONS="-XTypeApplications -XTemplateHaskell -XImportQualifiedPost -XPatternSynonyms -XBangPatterns"
          ${hlint}/bin/hlint $EXTENSIONS $FILES
        '';

      checkCabalFiles = src: pkgs.runCommand "check-cabal-files" { }
        ''
          touch $out
          echo "Check Cabal files in ${src}"
          FILES=$(find ${src} -name "*.cabal")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${cabal-fmt}/bin/cabal-fmt --check $FILES
        '';

      checkShellFiles = src: pkgs.runCommand "check-shell-files" { }
        ''
          touch $out
          echo "Check Shell files in ${src}"
          FILES=$(find ${src} -name "*.sh")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${shfmt}/bin/shfmt -d $FILES
          ${shellcheck}/bin/shellcheck $FILES
        '';

      checkDhallFiles = src: pkgs.runCommand "check-dhall-files" { }
        ''
          touch $out
          echo "Check Dhall files in ${src}"
          FILES=$(find ${src} -name "*.dhall")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${dhall}/bin/dhall format --check $FILES
        '';

      checkPurescriptFiles = src: pkgs.runCommand "check-purescript-files" { }
        ''
          touch $out
          echo "Check Purescript files in ${src}"
          FILES=$(find ${src} -name "*.purs")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${purs-tidy}/bin/purs-tidy check $FILES
        '';
    };
  fixers = pkgs:
    let
      nixpkgs-fmt = pkgs.nixpkgs-fmt;
      fourmolu = pkgs.haskellPackages.fourmolu;
      hlint = pkgs.hlint;
      cabal-fmt = pkgs.haskellPackages.cabal-fmt;
      shfmt = pkgs.shfmt;
      shellcheck = pkgs.shellcheck;
      dhall = pkgs.dhall;
      purs-tidy = easy-ps.purs-tidy;

    in
    {
      fixNixFiles = makeBundle {
        myScriptName = "fix-nix-files";
        myScript = ''
          echo "Fix Nix files in $@"
          FILES=$(find $@ -name "*.nix")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${nixpkgs-fmt}/bin/nixpkgs-fmt $FILES
        '';
      };

      fixHaskellFiles = makeBundle {
        myScriptName = "fix-haskell-files";
        myScript = ''
          echo "Fix Haskell files in $@"
          EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -XBangPatterns -o -fplugin=RecordDotPreprocessor"
          FILES=$(find $@ -name "*.hs")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${fourmolu}/bin/fourmolu -i $EXTENSIONS $FILES
        '';
      };

      fixCabalFiles = makeBundle {
        myScriptName = "fix-cabal-files";
        myScript = ''
          echo "Fix Cabal files in $@"
          FILES=$(find $@ -name "*.cabal")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${cabal-fmt}/bin/cabal-fmt --inplace $FILES
        '';
      };

      fixShellFiles = makeBundle {
        myScriptName = "fix-shell-files";
        myScript = ''
          echo "Fix Shell files in $@"
          FILES=$(find $@ -name "*.sh")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${shfmt}/bin/shfmt -w $FILES
        '';
      };

      fixDhallFiles = makeBundle {
        myScriptName = "fix-dhall-files";
        myScript = ''
          echo "Fix Dhall files in $@"
          FILES=$(find $@ -name "*.dhall")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${dhall}/bin/dhall format $FILES
        '';
      };

      fixPurescriptFiles = makeBundle {
        myScriptName = "fix-purescript-files";
        myScript = ''
          echo "Fix Purescript files in $@"
          FILES=$(find $@ -name "*.purs")
          if [ -z "$FILES" ]; then
             echo "No files found"
             exit 0;
          fi;
          ${purs-tidy}/bin/purs-tidy format-in-place $FILES
        '';
      };
    };
}
