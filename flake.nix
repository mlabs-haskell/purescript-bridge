{
  description = "Generate PureScript data types from Haskell data types";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]purescript-bridge \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
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
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        # TODO: Perhaps cleanSource
        src = ./.;

        # Nixpkgs from bot-plutus-interface
        inherit (inputs.bot-plutus-interface.inputs) nixpkgs;

        # Reliably cached
        pkgs = import nixpkgs { inherit system; };

        easy-ps = import inputs.easy-ps { inherit pkgs; };
        pursBridgeHsProjectFor = system: import ./nix/haskell.nix {
          inherit src system pkgs easy-ps;
          inputs = inputs // inputs.bot-plutus-interface.inputs;
          extraSources = inputs.bot-plutus-interface.extraSources;
        };
        pursBridgeFlakeFor = system: (pursBridgeHsProjectFor system).flake { };
        cq = import ./nix/code-quality.nix { projectName = ""; inherit pkgs easy-ps; };
        fileCheckers = cq.checkers pkgs;
      in
      {
        # Useful attributes
        inherit pkgs;
        pursBridgeFlake = pursBridgeFlakeFor system;

        # Flake standard attributes
        packages = self.pursBridgeFlake.${system}.packages;
        checks = self.pursBridgeFlake.${system}.checks;
        devShells = {
          "default" = self.pursBridgeFlake.${system}.devShell;
        };

        # Fix files
        fix-files = cq.format;

        # Used by CI
        build-all = pkgs.runCommand "build-all"
          (self.packages.${system} // self.devShells.${system})
          "touch $out";

        check-files = pkgs.runCommand "check-files"
          (builtins.mapAttrs (_: v: v src) fileCheckers)
          "touch $out";

      }
    );
}
