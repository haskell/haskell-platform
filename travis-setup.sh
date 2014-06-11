#!/bin/bash

GHC_TARBALL="$1"

set -ev

# Install HsColour
mkdir hscolour
pushd hscolour
cabal sandbox init
cabal install hscolour-1.20.3
sudo cp .cabal-sandbox/bin/HsColour /usr/local/bin/
popd

# Get GHC bin tarball
wget http://projects.haskell.org/haskell-platform/$GHC_TARBALL
