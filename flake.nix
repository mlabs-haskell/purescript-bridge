{
  description = "Generate PureScript data types from Haskell data types";
  inputs.haskell-nix.url = "github:mlabs-haskell/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.easy-ps = {
    url = "github:justinwoo/easy-purescript-nix";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskell-nix, easy-ps }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            purescript-bridge =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
        flake = pkgs.purescript-bridge.flake { };
      in
      flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."purescript-bridge:test:purescript-bridge";
        devShell = pkgs.purescript-bridge.shellFor {
          withHoogle = true;
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };

          exactDeps = true;

          buildInputs = with pkgs; with import easy-ps { inherit pkgs; }; [
            ghcid
            nixpkgs-fmt
            purs
            purescript-language-server
            spago
            haskellPackages.fourmolu
            nodejs
            dhall
            dhall-lsp-server
            nodejs
            nodePackages.node2nix
            nodePackages.jsonlint
            cabal-install
            hlint
            haskellPackages.hasktags
            haskellPackages.cabal-fmt
            nixpkgs-fmt
            shellcheck
            shfmt
          ];
        };
      });
}
