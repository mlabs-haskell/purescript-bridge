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
    plutip.url = "github:mlabs-haskell/plutip";

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

        pursBridgeHsProject = import ./nix/haskell.nix {
          inherit src system pkgs pkgs' easy-ps;
          inputs = inputs.bot-plutus-interface.inputs;
          extraSources = inputs.bot-plutus-interface.extraSources;
        };

        bpiInputs = inputs.plutip.inputs.bot-plutus-interface.inputs;

        # Nixpkgs from bot-plutus-interface
        # inherit (bpiInputs) nixpkgs;

        pursBridgeFlake = pursBridgeHsProject.flake { };
        cq = import ./nix/code-quality.nix { projectName = ""; inherit pkgs easy-ps; };
        fileCheckers = cq.checkers pkgs;

        # plutus-ledger-api Purescript typelib
        ledgerTypelib = import ./nix/purescript-bridge-typelib.nix {
          inherit pkgs;
          purs = easy-ps.purs-0_14_5; # TODO: Extract the purs version as a param and share across
          pursDir = ./plutus-ledger-api-typelib;
        };
      in
      {
        # Useful attributes
        inherit pkgs easy-ps ledgerTypelib pursBridgeFlake;

        # Flake standard attributes
        packages = pursBridgeFlake.packages // {
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
      }
    );
}
