#!/usr/bin/env bash

echo "Lint Haskell files"
# Extensions necessary to tell hlint about
EXTENSIONS="-XTypeApplications -XTemplateHaskell -XImportQualifiedPost -XPatternSynonyms -XBangPatterns"
SOURCES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
hlint $EXTENSIONS $SOURCES
CHECK_HASKELL=$?

echo "Lint Shell files"
SHELL_FILES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.sh$')
shellcheck $SHELL_FILES
CHECK_SHELL=$?

if [ $CHECK_HASKELL -ne 0 ] || [ $CHECK_SHELL -ne 0 ]; then
	echo "Linters complained."
	exit 1
fi
