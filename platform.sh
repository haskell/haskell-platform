#!/bin/sh


#for OS X in particular
export MACOSX_DEPLOYMENT_TARGET=10.7

set -e

HPTOOL=hptool/dist-newstyle/build/hptool-0.1/build/hptool/hptool
HPTOOL_ALT=hptool/.cabal-sandbox/bin/hptool

#if ( cabal sandbox --help >/dev/null 2>&1 ) ; then
#    if [ \! -d hptool/.cabal-sandbox ]
#    then
#        echo '***'
#        echo '*** Setting up sandbox for hptool'
#        echo '***'
#        cabal update
#        (cd hptool; cabal sandbox init; cabal install --only-dependencies)
#    fi
#else
#    if ( cabal install --dry-run --only-dependencies | grep -q 'would be installed' ) ; then
#        echo '=== pre-requisite packages for hptool are not installed'
#        echo '    run the following:'
#        echo '    cd hptool ; cabal install --only-dependencies'
#        exit 1
#    fi
#fi

echo '***'
echo '*** Building hptool'
echo '***'
(cd hptool; cabal new-build)

if [ "$HPTOOL_ALT" -nt "$HPTOOL" ] ; then
    HPTOOL="$HPTOOL_ALT"
fi

echo '***'
echo '*** Running hptool'
echo '***'
exec $HPTOOL "$@"
