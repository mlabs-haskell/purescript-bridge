{
  description = "Generate PureScript data types from Haskell data types";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]purescript-bridge \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix/2b4c58932d6c19a0c6b4bcde0a8c0581574c0d25";
    flake-utils.url = "github:numtide/flake-utils";
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    # Needed for crypto overlay
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };

    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/develop; # path:/<some_local_path>/cardano-transaction-lib;
    # We're reusing inputs from bot-plutus-interface as it's currently the source of nix truth.
    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface";

    # Our servant-purescript fork
    servant-purescript = {
      url = "github:mlabs-haskell/servant-purescript";
      flake = false;
    };

    plutus-extra = {
      url = "github:Liqwid-Labs/plutus-extra";
      flake = false;
    };
  };

  outputs = inputs@{ self, flake-utils, haskell-nix, ... }:
    let
      # Nixpkgs from bot-plutus-interface
      inherit (inputs.bot-plutus-interface.inputs) nixpkgs;
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      # TODO: Perhaps cleanSource
      src = ./.;
      # Reliably cached
      nixpkgsFor = system: import nixpkgs { inherit system; };
      # Poor caching due to overlay
      nixpkgsFor' = system: import nixpkgs {
        overlays =
          [ haskell-nix.overlay (import "${inputs.iohk-nix}/overlays/crypto") ];
        inherit system;
        inherit (haskell-nix) config;
      };
      easyPsFor = system: import inputs.easy-ps { pkgs = nixpkgsFor system; };
      pursVersion = "purs-0_14_5";

      haskellProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          easy-ps = easyPsFor system;
          # Haskell.nix project and flake
          # Filter out purescript-bridge
          extraSources' = builtins.filter
            (e:
              e.src.rev != "47a1f11825a0f9445e0f98792f79172efef66c00"
            )
            inputs.bot-plutus-interface.extraSources;
          # Use our servant-purescript fork
          extraSources'' = builtins.map
            (e:
              if e.src.rev == "44e7cacf109f84984cd99cd3faf185d161826963"
              then { src = inputs.servant-purescript; subdirs = e.subdirs; }
              else e
            )
            extraSources';

          extraSources =
            [
              {
                src = inputs.plutus-extra;
                subdirs = [
                  "quickcheck-plutus-instances"
                ];
              }
            ] ++ extraSources'';
        in
        import ./nix/haskell.nix {
          inherit src system pkgs pkgs' easy-ps extraSources;
          rtPurs =
            (roundTripTestPursFlakeFor system).packages.roundtrip-test-run-with-node;
        };

      # Code quality
      cqFor = system: import ./nix/code-quality.nix {
        pkgs = nixpkgsFor system;
        easy-ps = easyPsFor system;
      };

      fileCheckersFor = system: (cqFor system).checkers (nixpkgsFor system);

      fileFixersFor = system: (cqFor system).fixers (nixpkgsFor system);

      generatedLedgerPursFilesFor = system: (nixpkgsFor system).runCommand
        "generate-plutus-ledger-api-purs-files"
        {
          cli = (haskellProjectFor system).getComponent
            "purescript-bridge:exe:cli";
        }
        ''
          mkdir $out
          $cli/bin/cli generate-types --generated-dir $out
        '';

      # Generated plutus-ledger-api Purescript typelib
      generatedLedgerTypelibFor = system:
        let
          pkgs = nixpkgsFor system;
          spago = (easyPsFor system).spago;
          ctl = inputs.cardano-transaction-lib;
          purs = (easyPsFor system).${pursVersion};
          typelibName = "generated-ledger";
        in
        import ./nix/purescript-bridge.nix ctl {
          inherit pkgs purs spago typelibName;
          generatedPursFiles = generatedLedgerPursFilesFor system;
        };

      sampleLedgerTypelibFor = system:
        let
          pkgs = nixpkgsFor system;
          spago = (easyPsFor system).spago;
          ctl = inputs.cardano-transaction-lib;
          purs = (easyPsFor system).${pursVersion};
          typelibName = "sample-ledger";
        in
        import ./nix/purescript-bridge.nix ctl {
          inherit pkgs purs spago typelibName;
          generatedPursFiles = ./generated;
        };

      # Purescript - Haskell round trip generated typelib
      generatedRoundTripPursFilesFor = system: (nixpkgsFor system).runCommand
        "generate-roundtrip-test-purescript-files"
        {
          cli = (haskellProjectFor system).getComponent
            "purescript-bridge:exe:roundtrip";
        }
        ''
          mkdir $out
          $cli/bin/roundtrip generate-types --generated-dir $out
        '';

      generatedRoundTripTypelibFor = system:
        let
          pkgs = nixpkgsFor system;
          spago = (easyPsFor system).spago;
          ctl = inputs.cardano-transaction-lib;
          purs = (easyPsFor system).${pursVersion};
          typelibName = "roundtrip-test";
        in
        import ./nix/purescript-bridge.nix ctl {
          inherit pkgs purs spago typelibName;
          generatedPursFiles = generatedRoundTripPursFilesFor system;
        };

      # Purescript - Haskell round trip test purs flake
      roundTripTestPursFlakeFor = system:
        let
          pkgs = nixpkgsFor system;
          easy-ps = easyPsFor system;
          src = ./roundtrip/RoundTripPurs;
          workDir = "./roundtrip/RoundTripPurs";
          pursSubDirs = [ "/src" ];
          pursSubDirsTest = [ "/test" ];
          nodejs = pkgs.nodejs-14_x;
          spagoLocalPkgs = [
            inputs.cardano-transaction-lib
            (generatedRoundTripTypelibFor system)
          ];
          purs = easy-ps.${pursVersion};
          projectName = "roundtrip-test";
        in
        import ./nix/purescript-flake.nix {
          inherit src workDir pursSubDirs pursSubDirsTest pkgs system easy-ps
            spagoLocalPkgs nodejs purs projectName;
        };

      # purescript-bridge.nix flake
      purescriptBridgeNixFlakeFor = system:
        let
          pkgs = nixpkgsFor system;
          easy-ps = easyPsFor system;
          src = ./nix/purescript-bridge-nix-spago;
          workDir = "./nix/purescript-bridge-nix-spago";
          pursSubDirs = [ "/src" ];
          pursSubDirsTest = [ "/test" ];
          nodejs = pkgs.nodejs-14_x;
          spagoLocalPkgs = [ inputs.cardano-transaction-lib ];
          purs = easy-ps.${pursVersion};
          mainModule = "PureScriptBridge.Main";
          projectName = "purescript-bridge-nix";
        in
        import ./nix/purescript-flake.nix {
          inherit src workDir pursSubDirs pursSubDirsTest pkgs system
            easy-ps spagoLocalPkgs nodejs purs mainModule projectName;
        };
    in
    rec {
      haskellFlake = perSystem (system: (haskellProjectFor system).flake { });
      purescriptBridgeNixFlake = perSystem purescriptBridgeNixFlakeFor;
      roundTripTestPursFlake = perSystem roundTripTestPursFlakeFor;

      packages = perSystem (system:
        self.haskellFlake.${system}.packages
        // self.purescriptBridgeNixFlake.${system}.packages
        // self.roundTripTestPursFlake.${system}.packages
        // {
          sample-plutus-ledger-api-typelib = sampleLedgerTypelibFor system;
          plutus-ledger-api-typelib = generatedLedgerTypelibFor system;
        }
      );

      devShells = perSystem (system: {
        default = self.roundTripTestPursFlake.${system}.devShellComposeWith
          self.haskellFlake.${system}.devShell;
        typelibNix = self.purescriptBridgeNixFlake.${system}.devShell;
      });

      # Flake standard attributes
      checks = perSystem (system:
        self.haskellFlake.${system}.checks
        // self.purescriptBridgeNixFlake.${system}.checks
        // self.roundTripTestPursFlake.${system}.checks
      );

      hydraJobs = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          build-all = pkgs.runCommand "build-all"
            (self.packages.${system} // self.devShells.${system})
            "touch $out";
          fileCheckers = fileCheckersFor system;
          fileFixers = fileFixersFor system;
          test-all = pkgs.runCommand "check-all"
            {
              nativeBuildInputs = builtins.attrValues self.checks.${system};
            } "touch $out";
          check-files =
            let
            in
            pkgs.runCommand "check-files"
              ({
                checks = [
                  (fileCheckers.checkNixFiles src)
                  (fileCheckers.checkHaskellFiles src)
                  (fileCheckers.checkCabalFiles src)
                  (fileCheckers.checkShellFiles src)
                  (fileCheckers.checkDhallFiles src)
                  (fileCheckers.checkPurescriptFiles ./generated)
                  (fileCheckers.checkPurescriptFiles ./roundtrip/RoundTripPurs)
                ];
              })
              "touch $out";

          # Fix files
          fix-files = (cqFor system).makeBundle {
            myScriptName = "fix-files";
            myScript = ''
              ${fileFixers.fixNixFiles}/bin/fix-nix-files-bundle $@
              ${fileFixers.fixHaskellFiles}/bin/fix-haskell-files-bundle $@
              ${fileFixers.fixCabalFiles}/bin/fix-cabal-files-bundle $@
              ${fileFixers.fixShellFiles}/bin/fix-shell-files-bundle $@
              ${fileFixers.fixDhallFiles}/bin/fix-dhall-files-bundle $@
              ${fileFixers.fixPurescriptFiles}/bin/fix-purescript-files-bundle $@/generated
              ${fileFixers.fixPurescriptFiles}/bin/fix-purescript-files-bundle $@/roundtrip/RoundTripPurs
            '';
          };
        in
        {
          inherit checks fix-files check-files test-all;
        });
    };
}
