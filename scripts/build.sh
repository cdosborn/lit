#! /usr/bin/env bash

# Lit has several steps that have to occur for every fix. For example:
#
#   - Fix bug in src/*.lit
#   - Generate src/*.hs
#   - Rebuild with cabal 
#   - Run tests
#   - Update documentation (for the new source)
#
# This script does each of the above! Just run: `./build.sh`
# Note: documentation is built in a docs/ folder, which must be pushed to the
# gh-pages branch.

# Change into root lit directory
echo "● Changing into lit directory"
lit_home="$(dirname $(dirname $(readlink -f $0)))"
cd $lit_home

# Generate code (src files)
echo "● Regenerating *.hs from *.hs.lit:"
if [ -f ./dist/build/lit/lit ]; then 
    ./dist/build/lit/lit -n --code --code-dir=src/ src/*.lit || exit 1
else
    cabal configure && cabal build 
    ./dist/build/lit/lit -n --code --code-dir=src/ src/*.lit || exit 1
fi

# Run cabal
echo "● Building latest from src/*.hs:"
cabal configure && cabal build || exit 1

# Run tests
echo "● Running tests:"
scripts/test.sh || exit 1

# Generate docs (html)
echo "● Generating lit docs (see docs/):"
[ -d ./docs ] || mkdir docs
./dist/build/lit/lit --html --css=css/default.css --docs-dir=docs/ src/*.lit || exit 1
