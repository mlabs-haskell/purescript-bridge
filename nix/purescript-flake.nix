{ name
, src
, pursSubDirs ? [ "/src" "/test" ]
, pkgs
, system
, easy-ps
, spagoPkgs ? import (src + "/spago-packages.nix") { inherit pkgs; }
, spagoLocalPkgs ? [ ]
, nodejs
, nodePkgs ? import (src + "/node2nix.nix") { inherit pkgs system nodejs; }
, purs ? easy-ps.purs-0_14_5
}:

let
  nodePkgs' = nodePkgs // {
    shell = nodePkgs.shell.override {
      # see https://github.com/svanderburg/node2nix/issues/198
      buildInputs = [ pkgs.nodePackages.node-gyp-build ];
    };
  };
  nodeModules = nodePkgs'.shell.nodeDependencies;
  ps-lib = import ./purescript-lib.nix {
    inherit pkgs spagoPkgs spagoLocalPkgs nodejs nodeModules purs;
    spago = easy-ps.spago;
  };
  projectDir = src;
in
rec {
  defaultPackage = packages.${system}.${name};

  packages.${name} = ps-lib.buildPursProject {
    inherit projectDir pursSubDirs;
  };
  packages."${name}-bundle-commonjs" = ps-lib.bundlePursProjectCommonJs {
    inherit projectDir pursSubDirs;
  };

  checks."${name}-check" = ps-lib.runPursTest {
    inherit projectDir pursSubDirs;
  };

  check = pkgs.runCommand "combined-check"
    {
      nativeBuildInputs = builtins.attrValues packages.${name};

    } "touch $out";

  devShell = devShellComposeWith { shellHook = ""; buildInputs = [ ]; };
  devShellComposeWith = otherShell: pkgs.mkShell {
    buildInputs = (with easy-ps; [
      spago
      purs-tidy
      purescript-language-server
      pscid
      spago2nix
      psc-package
    ]) ++ (with pkgs; [
      dhall
      dhall-lsp-server
      nodejs # includes npm
      nodePackages.node2nix
      nodePackages.jsonlint
    ]) ++ [ purs ] ++ otherShell.buildInputs;

    inherit spagoLocalPkgs;

    phases = [ "installPhase" ];
    installPhase = ''touch $out'';

    shellHook = ''
      ${otherShell.shellHook}
      export XDG_CACHE_HOME=$TMPDIR
      export XDG_RUNTIME_DIR=$TMPDIR

      echo "Setting up Nodejs dependencies"
      ln -s ${nodeModules}/lib/node_modules $TMPDIR/node_modules
      export NODE_PATH="$TMPDIR/node_modules:$NODE_PATH"
      export PATH="$nodeModules/bin:$PATH"

      echo "Setting up local Spago packages"
      export LOCALS_DHALL=$TMPDIR/locals.dhall
      touch $LOCALS_DHALL
      cat <<DHALL > $LOCALS_DHALL
      let Location =
            https://prelude.dhall-lang.org/v15.0.0/Location/Type
              sha256:613ebb491aeef4ff06368058b4f0e6e3bb8a58d8c145131fc0b947aac045a529
      in
      DHALL
      for slp in $spagoLocalPkgs; do
        slpName=$(dhall repl <<< "($slp/spago.dhall).name" | grep \" | tr -d '"')
        cat <<DHALL >> $LOCALS_DHALL
          {
            $slpName = Location.Local "$slp/spago.dhall"
          } // ($slp/spago.dhall).packages //
      DHALL
      done
      echo "{=}" >> $LOCALS_DHALL
    '';
  };
}

