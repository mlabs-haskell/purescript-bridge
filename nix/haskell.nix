{ src, pkgs, easy-ps, system }:

pkgs.haskell-nix.project' {
  inherit src;
  compiler-nix-name = "ghc8107";
  modules = [
    ({ config, ... }: {
      packages = {
        allComponent.doHoogle = true;

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

    tools = { haskell-language-server = "latest"; };

    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
    '';
  };
  extraSources = [];
}
