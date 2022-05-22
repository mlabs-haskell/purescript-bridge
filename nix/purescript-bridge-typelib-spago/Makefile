all: clean spago2nix fix-files

clean:
	rm -fR node_modules .spago2nix/ .spago outputs .psci_modules

spago2nix: clean
	nix shell --impure --expr "let easy-ps = import (builtins.fetchGit "https://www.github.com/justinwoo/easy-purescript-nix") {}; in easy-ps.spago2nix" -c spago2nix generate

fix-files:
	@make -C ../../ $@
