
.PHONY: test develop test-all build-all build-test-all fix-files \
        check-files ci clean

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

NIX_BUILD:= nix -L --show-trace build
NIX_RUN:= nix -L --show-trace run
NIX_DEV:= nix -L --show-trace develop .\#default

develop:
	$(NIX_DEV)

# Tests
test:
	$(NIX_BUILD) .#test-all.${current-system}

test-rt:
	$(NIX_BUILD) .#checks.${current-system}.purescript-bridge:test:roundtrip-test

test-all: test

# Build all
build-all:
	$(NIX_BUILD) .#build-all.${current-system}

build-plutus-ledger-api-typelib:
	$(NIX_BUILD) .#packages.${current-system}.plutus-ledger-api-typelib

build-sample-plutus-ledger-api-typelib:
	$(NIX_BUILD) .#packages.${current-system}.sample-plutus-ledger-api-typelib

build-test-all: build-all test-all

# Fix files
fix-files: clean
	$(NIX_RUN) .#$@.${current-system} $$PWD

# Check files
check-files:
	$(NIX_BUILD) .#$@.${current-system}

update-all:
	nix -L flake lock --update-input cardano-transaction-lib
	nix -L --show-trace develop .#typelibNix -c make
	nix -L --show-trace develop .#default -c make

# Run what CI would
ci: check-files build-all test-rt

# Clean local folder.
clean:
	@ rm -rf dist-newstyle                  || true
	@ rm -rf .psc-ide-port                  || true
	@ rm -rf .spago || true
	@ make -C ./roundtrip/RoundTripPurs clean   || true
	@ make -C ./nix/purescript-bridge-nix-spago clean    || true

generate-plutus-ledger-api-typelib:
	@ if [ -d plutus-ledger-api-typelib ]; then git rm -r --cached plutus-ledger-api-typelib; else echo "skip"; fi
	@ if [ -d plutus-ledger-api-typelib ]; then rm -rf plutus-ledger-api-typelib; else echo "skip 1"; fi
	@ cabal run cli -- generate-plutus-ledger-api-types
	@ git add plutus-ledger-api-typelib
