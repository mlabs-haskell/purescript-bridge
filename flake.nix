{
  description = "Generate PureScript data types from Haskell data types";
  inputs.haskell-nix.url = "github:mlabs-haskell/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.easy-ps = {
    url = "github:justinwoo/easy-purescript-nix";
    flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        src = ./.;
        pkgs = import nixpkgs {
          inherit system;
          overlays =
            [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };
        easy-ps = import inputs.easy-ps { inherit pkgs; };
        pursBridgeHsProjectFor = system: import ./nix/haskell.nix { inherit system pkgs easy-ps src; };
        pursBridgeFlakeFor = system: (pursBridgeHsProjectFor system).flake { };
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
      }
    );
}
