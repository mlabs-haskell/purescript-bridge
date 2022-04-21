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

    # We're reusing inputs from bot-plutus-interface as it's currently the source of nix truth.
    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface";

    # Our servant-purescript fork
    servant-purescript = {
      url = "github:mlabs-haskell/servant-purescript";
      flake = false;
    };

  };

  outputs = inputs@{ self, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        # TODO: Perhaps cleanSource
        src = ./.;

        # Nixpkgs from bot-plutus-interface
        inherit (inputs.bot-plutus-interface.inputs) nixpkgs;

        # Reliably cached
        pkgs = import nixpkgs { inherit system; };

        # # Poor caching due to overlay
        pkgs' = import nixpkgs {
          overlays =
            [ haskell-nix.overlay (import "${inputs.iohk-nix}/overlays/crypto") ];
          inherit system;
          inherit (haskell-nix) config;
        };

        easy-ps = import inputs.easy-ps { inherit pkgs; };

        # Haskell.nix project and flake
        # Filter out purescript-bridge
        extraSources' = builtins.filter (e: e.src.rev != "47a1f11825a0f9445e0f98792f79172efef66c00") inputs.bot-plutus-interface.extraSources;
        # Use our servant-purescript fork
        extraSources'' = builtins.map
          (e:
            if e.src.rev == "44e7cacf109f84984cd99cd3faf185d161826963"
            then { src = inputs.servant-purescript; subdirs = e.subdirs; }
            else e)
          extraSources';
        pursBridgeHsProject = import ./nix/haskell.nix {
          inherit src system pkgs pkgs' easy-ps;
          inputs = inputs.bot-plutus-interface.inputs;
          extraSources = extraSources'';
        };
        pursBridgeFlake = pursBridgeHsProject.flake { };

        # Code quality
        cq = import ./nix/code-quality.nix { projectName = ""; inherit pkgs easy-ps; };
        fileCheckers = cq.checkers pkgs;

        # plutus-ledger-api Purescript typelib
        sampleLedgerTypelib = import ./nix/purescript-bridge-typelib.nix {
          inherit pkgs;
          purs = easy-ps.purs-0_14_5; # TODO: Extract the purs version as a param and share across
          pursDir = ./plutus-ledger-api-typelib;
        };
        ledgerTypelib = import ./nix/purescript-bridge-typelib.nix {
          inherit pkgs;
          purs = easy-ps.purs-0_14_5; # TODO: Extract the purs version as a param and share across
          pursDir = (pkgs.runCommand "generate-plutus-ledger-api-typelib"
            {
              cli = pursBridgeHsProject.getComponent "purescript-bridge:exe:cli";
            }
            ''
              mkdir $out
              $cli/bin/cli generate-plutus-ledger-api-types --purs-dir $out
            '');
        };

      in
      {
        # Useful attributes
        inherit pkgs easy-ps pursBridgeHsProject pursBridgeFlake;

        # Flake standard attributes
        packages = pursBridgeFlake.packages // {
          sample-plutus-ledger-api-typelib = sampleLedgerTypelib;
          plutus-ledger-api-typelib = ledgerTypelib;
        };
        checks = pursBridgeFlake.checks;
        devShells = {
          default = pursBridgeFlake.devShell;
        };

        # Fix files
        fix-files = cq.format;

        # Used by CI
        build-all = pkgs.runCommand "build-all"
          (self.packages.${system} // self.devShells.${system})
          "touch $out";

        check-files = pkgs.runCommand "check-files"
          (builtins.mapAttrs
            (k: v:
              if k == "checkPurescriptFiles"
              then v ./test/RoundTrip/app
              else v src)
            fileCheckers)
          "touch $out";

        # Purescript and bridge Nix libs
        lib = {
          bridgeTypelib = import ./nix/purescript-bridge-typelib.nix;
          pursFlake = import ./nix/purescript-flake.nix;
          pursLib = import ./nix/purescript-lib.nix;
        };
      }
    );
}
