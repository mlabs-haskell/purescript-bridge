all: clean update-deps fix-files

clean:
	rm -fR node_modules .spago2nix/ .spago output .psci_modules generated

# Run this under `nix develop .#typelibNix`
update-deps: clean
	spago2nix generate
	npm i --package-lock-only
	node2nix -l package-lock.json -d -c node2nix.nix

fix-files:
	@make -C ../../ $@
