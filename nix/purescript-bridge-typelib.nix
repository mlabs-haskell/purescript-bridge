ctl: { pkgs, generatedPursFiles, purs, spago }:
let
  spagoProjectDir = pkgs.runCommand "spago-project-dir"
    { }
    ''
      mkdir $out
      mkdir $out/generated
      cp ${./purescript-bridge-typelib-spago}/* $out
      cp -r ${generatedPursFiles}/* $out/generated/
    '';

  # Node
  nodeModules = [ ];
  nodejs = pkgs.nodejs-14_x;

  # Spago
  spagoPkgs = import (./purescript-bridge-typelib-spago/spago-packages.nix) { inherit pkgs; };
  spagoLocalPkgs = [ ctl ];

  # Purescript
  pursLib = import ./purescript-lib.nix {
    inherit pkgs nodejs nodeModules spago spagoPkgs spagoLocalPkgs purs;
  };

in
pursLib.buildPursProject {
  projectDir = spagoProjectDir;
  pursSubDirs = [ "/generated" ];
  checkWarnings = false;
}
