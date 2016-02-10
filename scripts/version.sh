#! /usr/bin/env bash
#
# Script to assist in versioning the project
#

CURRENT_VERSION=$(grep -Eo "[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+?" src/lit.hs.lit)
FILES_TO_EDIT=$(grep -rl "$CURRENT_VERSION" src/*lit lit.cabal)
echo "What is the next version? (prev $CURRENT_VERSION)"
read -e NEW_VERSION
echo "# Run this line to replace the version easily"
echo "     vim -c  \"execute 'bufdo %s:$CURRENT_VERSION:$NEW_VERSION:gc | w' | qa\"" $FILES_TO_EDIT
echo "# Rebuild lit/package/upload"
echo "     ./scripts/build.sh;"
echo "     cabal sdist"
echo "     cabal upload ./dist/lit-$NEW_VERSION.tar.gz"
