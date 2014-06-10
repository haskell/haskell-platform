#!/bin/bash

GHC_VER=7.8.2
GHC_TARBALL=ghc-7.8.2-x86_64-unknown-linux-deb7.tar.xz

set -ev
export PATH="~/.cabal/bin:$PATH"

# Bootstrap cabal with sandbox via cabal-dev
cabal update
cabal install cabal-dev
mkdir cabal-install
pushd cabal-install
cabal-dev install cabal-install-1.20.0.2
sudo cp cabal-dev/bin/cabal /usr/local/bin/
popd

# Install HsColour
mkdir hscolour
pushd hscolour
cabal sandbox init
cabal install hscolour-1.20.3
sudo cp .cabal-sandbox/bin/HsColour /usr/local/bin/

# Get GHC bin tarball
wget http://www.haskell.org/ghc/dist/$GHC_VER/$GHC_TARBALL
