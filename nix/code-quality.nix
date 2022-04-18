{ projectName, pkgs, easy-ps }:
let

  # Recursively build script contents from a list of script addresses. They get
  # concatenated sequentially.
  mkScriptContents = sPs:
    if sPs == [ ]
    then ""
    else (with builtins;(readFile (head sPs)) + "\n" + (mkScriptContents (tail sPs)));

  # Util fn.
  makeCIAction = { action, scriptPaths, buildInputsAdditional }:
    pkgs.symlinkJoin rec {

      name = "${action}-${projectName}";

      scriptContents = mkScriptContents scriptPaths;

      script =
        (pkgs.writeScriptBin name scriptContents).overrideAttrs
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
  dependencies = {

    format = [
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.cabal-fmt
      pkgs.nixpkgs-fmt
      pkgs.shfmt
      pkgs.dhall
      easy-ps.purs-tidy
    ];

    lint = [
      pkgs.hlint
      pkgs.haskellPackages.apply-refact
      pkgs.shellcheck
    ];
  };

  # Shell scripts.
  sScript = {
    check-formatting = ./scripts/check-formatting.sh;
    format = ./scripts/format.sh;
    lint = ./scripts/lint.sh;
    lint-inplace = ./scripts/lint-inplace.sh;
  };

in
{
  "format" = makeCIAction {
    action = "format";
    scriptPaths = [ sScript.format ];
    buildInputsAdditional = dependencies.format;
  };

  "lint-inplace" = makeCIAction {
    action = "lint-inplace";
    scriptPaths = [ sScript.lint-inplace ];
    buildInputsAdditional = dependencies.lint;
  };

  "format-lint" = makeCIAction {
    action = "format-lint";
    scriptPaths = [ sScript.format sScript.lint-inplace ];
    buildInputsAdditional = dependencies.lint ++ dependencies.format;
  };


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
          echo "Check Nix files"
          NIX_FILES=$(find ${src} -name "*.nix")
          ${nixpkgs-fmt}/bin/nixpkgs-fmt --check $NIX_FILES
        '';

      checkHaskellFiles = src: pkgs.runCommand "check-haskell-files" { }
        ''
          touch $out
          echo "Check Haskell files"
          EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -XBangPatterns -o -fplugin=RecordDotPreprocessor"
          HASKELL_FILES=$(find ${src} -name "*.hs")
          ${fourmolu}/bin/fourmolu --mode check --check-idempotence $EXTENSIONS $HASKELL_FILES
          if [ $? -ne 0 ]; then
            echo "Fourmolu complained."
            exit 1
          fi
          EXTENSIONS="-XTypeApplications -XTemplateHaskell -XImportQualifiedPost -XPatternSynonyms -XBangPatterns"
          ${hlint}/bin/hlint $EXTENSIONS $HASKELL_FILES
        '';

      checkCabalFiles = src: pkgs.runCommand "check-cabal-files" { }
        ''
          touch $out
          echo "Check Cabal files"
          CABAL_FILES=$(find ${src} -name "*.cabal")
          ${cabal-fmt}/bin/cabal-fmt --check $CABAL_FILES
        '';

      checkShellFiles = src: pkgs.runCommand "check-shell-files" { }
        ''
          touch $out
          echo "Check Shell files"
          SHELL_FILES=$(find ${src} -name "*.sh")
          ${shfmt}/bin/shfmt -d $SHELL_FILES
          ${shellcheck}/bin/shellcheck $SHELL_FILES
        '';

      checkDhallFiles = src: pkgs.runCommand "check-dhall-files" { }
        ''
          touch $out
          echo "Check Dhall files"
          DHALL_FILES=$(find ${src} -name "*.dhall")
          ${dhall}/bin/dhall format --check $DHALL_FILES
        '';

      checkPurescriptFiles = src: pkgs.runCommand "check-purescript-files" { }
        ''
          touch $out
          echo "Check Purescript files"
          PURS_FILES=$(find ${src} -name "*.purs")
          ${purs-tidy}/bin/purs-tidy check $PURS_FILES
        '';
    };
}




