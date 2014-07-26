#!/bin/sh

set -e

HPTOOL=hptool/dist/build/hptool/hptool

if [ \! \( -e $HPTOOL -a -x $HPTOOL \) ]
then
    if [ \! -d hptool/.cabal-sandbox ]
    then
        echo '***'
        echo '*** Setting up sandbox for hptool'
        echo '***'
        cabal update
        (cd hptool; cabal sandbox init; cabal install --only-dependencies)
    fi

    echo '***'
    echo '*** Building hptool'
    echo '***'
    (cd hptool; cabal build)
fi

echo '***'
echo '*** Running hptool'
echo '***'
$HPTOOL "$@"
