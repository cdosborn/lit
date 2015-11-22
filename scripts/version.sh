#! /usr/bin/env bash

# Don't forget to update version
echo Make sure \'lit -v\' and lit.cabal have same version
grep -n 'Version' src/lit.hs.lit
grep -n '^version' lit.cabal
echo "Confirm the two version numbers agree? (Y/n)"
read agree

if [[ $agree =~ [yY]+ ]]; then
    # Generate hackage package ;)
    cabal sdist

    # Upload package to hackage
    cabal upload ./dist/*.tar.gz
else
    echo "Update the version before building"
    echo "vim -o src/lit.hs.lit lit.cabal"
    exit 1
fi
