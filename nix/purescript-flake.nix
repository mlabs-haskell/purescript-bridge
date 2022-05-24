{ name
, src
, pursSubDirs ? [ "/src" ]
, pursSubDirsTest ? [ "/test" ]
, workDir # FIXME: This is awkward; to cd into and attach a locals.dhall link
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
    inherit projectDir;
    pursSubDirs = pursSubDirs ++ pursSubDirsTest;
  };

  check = pkgs.runCommand "combined-check"
    {
      nativeBuildInputs = builtins.attrValues packages.${name};

    } "touch $out";

  devShell = devShellComposeWith (pkgs.mkShell { installPhase = ""; });
  devShellComposeWith = otherShell: otherShell.overrideAttrs (old:
    {
      buildInputs = pkgs.lib.lists.unique ((with easy-ps; [
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
      ]) ++ [ purs ] ++ old.buildInputs);

      phases = pkgs.lib.lists.unique ([ "installPhase" ] ++ old.phases);
      installPhase = ''
        ${old.installPhase}
        if [ ! -f $out ]; then
           touch $out;
        fi
      '';
      shellHook = ''
        ${old.shellHook}
        export XDG_CACHE_HOME=$TMPDIR
        export XDG_RUNTIME_DIR=$TMPDIR

        echo "Setting up Nodejs dependencies"
        ln -s ${nodeModules}/lib/node_modules $TMPDIR/node_modules
        export NODE_PATH="$TMPDIR/node_modules:$NODE_PATH"
        export PATH="$nodeModules/bin:$PATH"

        echo "Setting up local Spago packages"
        cd ${workDir}
        ln -fs ${ps-lib.localsDhall} ./locals.dhall
      '';
    });
}

