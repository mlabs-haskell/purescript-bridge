
.PHONY: test develop test-all build-all build-test-all fix-files \
        check-files ci clean

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

NIX_BUILD:= nix -L --show-trace build
NIX_RUN:= nix -L --show-trace run
NIX_DEV:= nix -L --show-trace develop .#default
NIX_DEV_RT:= nix -L --show-trace develop .#roundTripTest

develop:
	$(NIX_DEV)

# Tests
test:
	$(NIX_BUILD) .#checks.${current-system}."purescript-bridge:test:tests"

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

# Run what CI would
ci: check-files build-all
	$(NIX_DEV_RT) -c cabal run test:tests

# Clean local folder.
clean:
	@ rm -rf dist-newstyle                  || true
	@ rm -rf .psc-ide-port                  || true
	@ rm -rf ./test/RoundTrip/app/dist    || true
	@ rm -rf ./test/RoundTrip/app/output    || true
	@ rm -rf ./test/RoundTrip/app/.spago    || true
	@ rm -rf ./test/RoundTrip/app/.psci_modules    || true
	@ rm -rf ./test/RoundTrip/app/.spago2nix    || true
	@ rm -rf ./test/RoundTrip/app/node_modules || true
	@ rm -rf ./test/RoundTrip/app/generated || true
	@ rm -rf .spago || true
	@ rm -rf ./nix/purescript-bridge-typelib-spago/.spago2nix || true
	@ rm -rf ./nix/purescript-bridge-typelib-spago/output || true
	@ rm -rf ./nix/purescript-bridge-typelib-spago/.spago || true
	@ rm -rf ./nix/purescript-bridge-typelib-spago/node_modules || true

generate-plutus-ledger-api-typelib:
	@ if [ -d plutus-ledger-api-typelib ]; then git rm -r --cached plutus-ledger-api-typelib; else echo "skip"; fi
	@ if [ -d plutus-ledger-api-typelib ]; then rm -rf plutus-ledger-api-typelib; else echo "skip 1"; fi
	@ cabal run cli -- generate-plutus-ledger-api-types
	@ git add plutus-ledger-api-typelib
