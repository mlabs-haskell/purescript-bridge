ctl: { pkgs, typelibName, generatedPursFiles, purs, spago }:
let
  spagoProjectDir = pkgs.runCommand "${typelibName}-spago-project-dir"
    { }
    ''
      mkdir $out
      mkdir $out/src
      cp -r ${./purescript-bridge-nix-spago}/* $out
      cp -r ${generatedPursFiles}/* $out/src/
    '';

  # Node
  nodeModules = [ ];
  nodejs = pkgs.nodejs-14_x;

  # Spago
  spagoPkgs = import (./purescript-bridge-nix-spago/spago-packages.nix) { inherit pkgs; };
  spagoLocalPkgs = [ ctl ];

  # Purescript
  pursLib = import ./purescript-lib.nix {
    inherit pkgs nodejs nodeModules spago spagoPkgs spagoLocalPkgs purs;
    mainModule = "PureScriptBridge.Main";
    projectName = "${typelibName}-typelib";
  };

in
pursLib.buildPursProject {
  projectDir = spagoProjectDir;
  pursSubDirs = [ "/src" ];
  checkWarnings = false;
}
