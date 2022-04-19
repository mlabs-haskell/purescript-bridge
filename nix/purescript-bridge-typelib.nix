{ pkgs, pursDir, purs }:
with import ./purescript-bridge-typelib-spago/spago-packages.nix { inherit pkgs; };
pkgs.stdenv.mkDerivation {
  name = "purescript-bridge-typelib-spago-build";
  src = ./purescript-bridge-typelib-spago;
  buildInputs = [ purs installSpagoStyle buildFromNixStore ];
  doCheck = true;
  buildPhase = ''
    mkdir src
    cp -r ${pursDir} src/
  '';
  checkPhase = ''
    install-spago-style
    build-from-store $(find src -name "*.purs")
  '';
  installPhase = ''
    mkdir $out
    cp -r * $out
  '';
}
