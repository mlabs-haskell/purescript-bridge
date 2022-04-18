#!/usr/bin/env bash
echo "Format Haskell files"
# Extensions necessary to tell fourmolu about
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -XBangPatterns -o -fplugin=RecordDotPreprocessor"
HASKELL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
fourmolu -i $EXTENSIONS $HASKELL_FILES

echo "Format Cabal files"
CABAL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.cabal')
cabal-fmt --inplace $CABAL_FILES

echo "Format Nix files"
NIX_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix')
nixpkgs-fmt $NIX_FILES

echo "Format Shell files"
SHELL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.sh$')
shfmt -w $SHELL_FILES

echo "Format Dhall files"
DHALL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.dhall')
dhall format $DHALL_FILES

echo "Format Purescript files"
PURS_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.purs')
purs-tidy format-in-place $PURS_FILES
