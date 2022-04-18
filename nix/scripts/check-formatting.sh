#!/usr/bin/env bash
echo "Check Haskell file formatting"
# Extensions necessary to tell fourmolu about
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -XBangPatterns -o -fplugin=RecordDotPreprocessor"
HASKELL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
fourmolu --mode check --check-idempotence $EXTENSIONS $HASKELL_FILES
CHECK_HASKELL=$?

echo "Check Cabal file formatting"
CABAL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.cabal')
cabal-fmt --check $CABAL_FILES
CHECK_CABAL=$?

echo "Check Nix file formatting"
NIX_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix')
nixpkgs-fmt --check $NIX_FILES
CHECK_NIX=$?

echo "Check Shell file formatting"
SHELL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.sh$')
shfmt -d $SHELL_FILES
CHECK_SHELL=$?

echo "Check Dhall file formatting"
DHALL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.dhall')
dhall format --check $DHALL_FILES
CHECK_DHALL=$?

echo "Check Purescript file formatting"
PURS_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.purs')
purs-tidy check $PURS_FILES
CHECK_PURS=$?

if [ $CHECK_HASKELL -ne 0 ] || [ $CHECK_CABAL -ne 0 ] || [ $CHECK_NIX -ne 0 ] || [ $CHECK_SHELL -ne 0 ] || [ $CHECK_DHALL -ne 0 ] || [ $CHECK_PURS -ne 0 ]; then
	echo "Formatting checks failed. Did you forget to *make format*?"
	exit 1
fi
