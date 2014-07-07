#!/bin/sh

cd "/Library/Haskell/ghc-{{ghcVersion}}-{{arch}}/bin"

./activate-hs "{{ghcVersion}}-{{arch}}"
./uninstall-hs install-check "{{ghcVersion}}" Finder

sudo -n -u $USER open /Library/Haskell/doc/start.html
