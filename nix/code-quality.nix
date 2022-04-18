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

  "check-formatting" = makeCIAction {
    action = "check-formatting";
    scriptPaths = [ sScript.check-formatting ];
    buildInputsAdditional = dependencies.format;
  };

  "lint" = makeCIAction {
    action = "lint";
    scriptPaths = [ sScript.lint ];
    buildInputsAdditional = dependencies.lint;
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
}
