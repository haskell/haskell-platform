#!/bin/bash

GHC_VER=7.8.2
GHC_TARBALL=ghc-7.8.2-x86_64-unknown-linux-deb7.tar.xz

set -ev

# Install HsColour
mkdir hscolour
pushd hscolour
cabal sandbox init
cabal install hscolour-1.20.3
sudo cp .cabal-sandbox/bin/HsColour /usr/local/bin/
popd

# Get GHC bin tarball
wget http://www.haskell.org/ghc/dist/$GHC_VER/$GHC_TARBALL
