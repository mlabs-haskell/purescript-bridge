{ src, system, pkgs, pkgs', easy-ps, extraSources, rtPurs }:
pkgs'.haskell-nix.cabalProject' {
  inherit src;
  name = "purescript-bridge";
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

        # Required for RoundTrip test
        purescript-bridge.components.tests.roundtrip-test.build-tools =
          [
            rtPurs
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

    # We use the ones from vanilla Nixpkgs, since they are cached reliably.
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
      # RoundTrip Purescript app
      rtPurs
    ];

    # Add here so `cabal build` can find them
    additional = ps: [ ps.plutus-tx ps.plutus-ledger-api ps.quickcheck-plutus-instances ];

    tools = { haskell-language-server = "latest"; };

    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
    '';
  };
}
