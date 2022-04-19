{ src, inputs, pkgs, pkgs', extraSources, system, dev ? "-f-dev", doHaddock ? false }:

pkgs.haskell-nix.cabalProject' {
  inherit src;
  compiler-nix-name = "ghc8107";
  cabalProjectFileName = "cabal.project";
  modules = [
    ({ config, ... }: {
      packages = {
        allComponent.doHoogle = true;
        plutus-ledger.doHaddock = doHaddock;
        plutus-ledger.flags.defer-plugin-errors = doHaddock;
        # plutus-contract.doHaddock = doHaddock;
        # plutus-contract.flags.defer-plugin-errors = doHaddock;
        # plutus-use-cases.doHaddock = doHaddock;
        # plutus-use-cases.flags.defer-plugin-errors = doHaddock;

        # cardano-crypto-praos.components.library.pkgconfig =
        #   pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

        # cardano-crypto-class.components.library.pkgconfig =
        #   pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

        # cardano-wallet-core.components.library.build-tools =
        #   [ pkgs.buildPackages.buildPackages.gitMinimal ];

        # cardano-config.components.library.build-tools =
        #   [ pkgs.buildPackages.buildPackages.gitMinimal ];

        # # Required for Plutip tests
        # cardax-streaming-merge-plutus.components.tests.cardax-streaming-merge-plutus-test.build-tools =
        #   [
        #     config.hsPkgs.cardano-cli.components.exes.cardano-cli
        #     config.hsPkgs.cardano-node.components.exes.cardano-node
        #   ];

        # cardax-streaming-merge-plutus.components.exes.cardax-streaming-merge-plutus-test.build-tools =
        #   [
        #     config.hsPkgs.cardano-cli.components.exes.cardano-cli
        #     config.hsPkgs.cardano-node.components.exes.cardano-node
        #   ];

        # # Don't build in dev
        # cardax-streaming-merge-plutus.components.library.configureFlags = [ dev ];
        # cardax-streaming-merge-plutarch.components.library.configureFlags = [ dev ];
        # cardax-streaming-merge-pure.components.library.configureFlags = [ dev ];
        # cardax-streaming-merge-plutus.components.exes.cardax-dex-cli.configureFlags = [ dev ];
        # cardax-streaming-merge-plutus.components.tests.cardax-streaming-merge-plutus-test.configureFlags = [ dev ];
        # cardax-streaming-merge-plutus.components.exes.cardax-streaming-merge-plutus-test.configureFlags = [ dev ];

      };

    })
  ];

  shell = {

    withHoogle = true;

    exactDeps = true;

    # We use the ones from Nixpkgs, since they are cached reliably.
    # Eventually we will probably want to build these with haskell.nix.
    nativeBuildInputs = with pkgs'; [
      cabal-install
      hlint
      haskellPackages.fourmolu
      haskellPackages.hasktags
      haskellPackages.cabal-fmt
      haskellPackages.apply-refact
      nixpkgs-fmt
      shellcheck
      shfmt
    ];

    tools = { haskell-language-server = "latest"; };

    additional = ps:
      with ps; [
      ];

    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
    '';
  };
  extraSources = extraSources;
}
