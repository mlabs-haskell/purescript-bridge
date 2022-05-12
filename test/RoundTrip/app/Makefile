SHELL := bash
.ONESHELL:
.PHONY: clean update-deps
.SHELLFLAGS := -eu -o pipefail -c

clean:
	rm -fR node_modules .spago2nix/ .spago outputs .psci_modules

update-deps: clean
	spago2nix generate && npm i --package-lock-only && node2nix -l package-lock.json -d -c node2nix.nix
