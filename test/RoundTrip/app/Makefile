all: clean update-deps format

clean:
	rm -fR node_modules .spago2nix/ .spago output .psci_modules generated

update-deps: clean
	spago2nix generate
	npm i --package-lock-only
	node2nix -l package-lock.json -d -c node2nix.nix

format: spago-packages.nix node2nix.nix node-env.nix node-packages.nix
	nixpkgs-fmt *.nix
