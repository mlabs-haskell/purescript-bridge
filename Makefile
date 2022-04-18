current-system := $(shell nix eval --impure --expr builtins.currentSystem)

NIX_BUILD:= nix -L --show-trace build
NIX_RUN:= nix -L --show-trace run

# Tests
test:
	$(NIX_BUILD) .#checks.${current-system}.purescript-bridge:test:tests

test-all: test

# Build all
build-all:
	$(NIX_BUILD) .#build-all.${current-system}

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
