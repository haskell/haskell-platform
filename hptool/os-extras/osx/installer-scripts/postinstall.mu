#!/bin/sh

cd "/Library/Haskell/ghc-{{ghcVersion}}-{{arch}}/bin"

./activate-hs "{{ghcVersion}}-{{arch}}"
./uninstall-hs install-check "{{ghcVersion}}" Finder

if [ "${COMMAND_LINE_INSTALL}" == '1' ] ; then
    echo "View documentation with this command:"
    echo "    open /Library/Haskell/doc/start.html"
else
	sudo -n -u $USER open /Library/Haskell/doc/start.html
fi
