#!/usr/bin/env bash

echo "Linting Haskell files and replacing them inplace."

# Extensions necessary to tell hlint about
EXTENSIONS="-XTypeApplications -XTemplateHaskell -XImportQualifiedPost -XPatternSynonyms -XBangPatterns"
SOURCES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')

hlint $EXTENSIONS $SOURCES

if [ $? -eq 1 ]; then
	echo $SOURCES | xargs -t -n 1 hlint --refactor --refactor-options="--inplace" $EXTENSIONS
fi
