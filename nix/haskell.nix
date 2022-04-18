{ src, system, pkgs, easy-ps, inputs, extraSources }:
let
  # Poor caching due to overlay
  pkgs' = import inputs.nixpkgs {
    overlays =
      [ inputs.haskell-nix.overlay (import "${inputs.iohk-nix}/overlays/crypto") ];
    inherit system;
    inherit (inputs.haskell-nix) config;
  };
in
pkgs'.haskell-nix.cabalProject' {
  inherit src;
  compiler-nix-name = "ghc8107";
  cabalProjectFileName = "cabal.project";
  modules = [
    ({ config, ... }: {
      packages = {
        allComponent.doHoogle = true;

        # Massaging the compilation
        plutus-ledger.doHaddock = false;
        plutus-ledger.flags.defer-plugin-errors = false;
        plutus-contract.doHaddock = false;
        plutus-contract.flags.defer-plugin-errors = false;
        plutus-use-cases.doHaddock = false;
        plutus-use-cases.flags.defer-plugin-errors = false;

        cardano-crypto-praos.components.library.pkgconfig =
          pkgs'.lib.mkForce [ [ pkgs'.libsodium-vrf ] ];

        cardano-crypto-class.components.library.pkgconfig =
          pkgs'.lib.mkForce [ [ pkgs'.libsodium-vrf ] ];

        cardano-wallet-core.components.library.build-tools =
          [ pkgs'.buildPackages.buildPackages.gitMinimal ];

        cardano-config.components.library.build-tools =
          [ pkgs'.buildPackages.buildPackages.gitMinimal ];

        # Required for Spago based `around` tests
        purescript-bridge.components.tests.tests.build-tools =
          [
            easy-ps.purs-0_14_5
            easy-ps.spago
            pkgs.nodejs-12_x
          ];

        # Don't build in dev
        # TODO: Add purescript-bridge.components.library.configureFlags = [ dev ];

      };

    })
  ];

  inherit extraSources;

  shell = {

    withHoogle = true;

    exactDeps = true;

    # We use the ones from Nixpkgs, since they are cached reliably.
    # Eventually we will probably want to build these with haskell.nix.
    nativeBuildInputs = with pkgs; [
      # Building code
      cabal-install
      easy-ps.spago
      easy-ps.spago2nix
      easy-ps.psc-package
      dhall
      pkgs.nodejs-12_x # includes npm
      nodePackages.node2nix
      easy-ps.purs-0_14_5
      # Code assistance
      easy-ps.purescript-language-server
      easy-ps.pscid
      dhall-lsp-server
      # Code quality
      # Haskell/Cabal
      hlint
      haskellPackages.fourmolu
      haskellPackages.hasktags
      haskellPackages.cabal-fmt
      haskellPackages.apply-refact
      # Nix
      nixpkgs-fmt
      # Shell
      shellcheck
      shfmt
      # Purescript
      easy-ps.purs-tidy
      # JSON
      nodePackages.jsonlint
    ];

    # Add here so `cabal build` can find them
    additional = ps: [ ps.plutus-tx ps.plutus-ledger-api ];

    tools = { haskell-language-server = "latest"; };

    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
    '';
  };
}
