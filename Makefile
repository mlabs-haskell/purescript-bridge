
.PHONY: test develop test-all build-all build-test-all fix-files \
        check-files ci clean

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

NIX_BUILD:= nix -L --show-trace build
NIX_RUN:= nix -L --show-trace run

develop:
	nix develop -L .#default

# Tests
test:
	$(NIX_BUILD) .#checks.${current-system}."purescript-bridge:test:tests"

test-all: test

# Build all
build-all:
	$(NIX_BUILD) .#build-all.${current-system}

build-plutus-ledger-api-typelib:
	$(NIX_BUILD) .#packages.${current-system}.plutus-ledger-api-typelib

build-plutus-sample-ledger-api-typelib:
	$(NIX_BUILD) .#packages.${current-system}.sample-plutus-ledger-api-typelib

build-test-all: build-all test-all

# Fix files
fix-files:
	$(NIX_RUN) .#$@.${current-system}

# Check files
check-files:
	$(NIX_BUILD) .#$@.${current-system}

# Run what CI would
ci: check-files build-all

# Clean local folder.
clean:
	@ rm -rf dist-newstyle                  || true
	@ rm -rf .psc-ide-port                  || true
	@ rm -rf ./test/RoundTrip/app/dist    || true
	@ rm -rf ./test/RoundTrip/app/output    || true
	@ rm -rf ./test/RoundTrip/app/.spago    || true
	@ rm -rf ./test/RoundTrip/app/.psci_modules    || true
	@ rm -rf ./test/RoundTrip/app/.spago2nix    || true

generate-plutus-ledger-api-typelib: plutus-ledger-api-typelib
	@ git rm -r --cached plutus-ledger-api-typelib
	@ rm -fR plutus-ledger-api-typelib
	@ cabal run cli -- generate-plutus-ledger-api-types
