{
  description = "Generate PureScript data types from Haskell data types";
  nixConfig.bash-prompt =
    "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]purescript-bridge \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";
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

  };

  outputs = inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        src = self;

        # Nixpkgs from bot-plutus-interface
        inherit (inputs.bot-plutus-interface.inputs) nixpkgs;

        # Reliably cached
        pkgs = import nixpkgs { inherit system; };

        easy-ps = import inputs.easy-ps { inherit pkgs; };

        pursBridgeHsProject = import ./nix/haskell.nix {
          inherit src system pkgs easy-ps;
          inputs = inputs // inputs.bot-plutus-interface.inputs;
          extraSources = inputs.bot-plutus-interface.extraSources;
        };

        pursBridgeFlake = pursBridgeHsProject.flake { };

        cq = import ./nix/code-quality.nix {
          projectName = "purescript-bridge-code-quality";
          inherit pkgs easy-ps;
        };

        fileCheckers = cq.checkers pkgs;

        # plutus-ledger-api Purescript typelib
        ledgerTypelib = import ./nix/purescript-bridge-typelib.nix {
          inherit pkgs;
          # TODO: Extract the purs version as a param and share across
          purs = easy-ps.purs-0_14_5;
          cli = pursBridgeHsProject.getComponent "purescript-bridge:exe:cli";
        };

      in {
        # Useful attributes
        inherit pkgs ledgerTypelib pursBridgeFlake pursBridgeHsProject;

        # Flake standard attributes
        packages = self.pursBridgeFlake.${system}.packages;
        checks = self.pursBridgeFlake.${system}.checks;
        devShells = { default = self.pursBridgeFlake.${system}.devShell; };
        fix-files = cq.format; # Fix files
        build-all = pkgs.runCommand "build-all"
          (self.packages.${system} // self.devShells.${system}) "touch $out"; # Used by CI
        check-files = pkgs.runCommand "check-files"
          (builtins.mapAttrs (_: v: v src) fileCheckers) "touch $out";
      });
}
