#!/bin/sh
#
# There are two ways to invoke this script:
#
# 1) ./platform.sh GHC_BINDIST CABAL STACK build-all
#
# 2) Create the file config.platform with the contents:
#
#    GHC_BINDIST=...
#    CABAL=...
#    STACK=...
#
# and then run this script as:
#
#    ./platform.sh build-all
#
set -e

HPTOOL=hptool/dist/build/hptool/hptool
HPTOOL_ALT=hptool/.cabal-sandbox/bin/hptool

if ( cabal sandbox --help >/dev/null 2>&1 ) ; then
    if [ \! -d hptool/.cabal-sandbox ]
    then
        echo '***'
        echo '*** Setting up sandbox for hptool'
        echo '***'
        cabal update
        (cd hptool; cabal sandbox init; cabal install --only-dependencies)
    fi
else
    if ( cabal install --dry-run --only-dependencies | grep -q 'would be installed' ) ; then
        echo '=== pre-requisite packages for hptool are not installed'
        echo '    run the following:'
        echo '    cd hptool ; cabal install --only-dependencies'
        exit 1
    fi
fi

echo '***'
echo '*** Building hptool'
echo '***'
(cd hptool; cabal build)

if [ "$HPTOOL_ALT" -nt "$HPTOOL" ] ; then
    HPTOOL="$HPTOOL_ALT"
fi

SCRIPT=$(readlink -f "$0")
SCRIPT_DIR=$(dirname "$SCRIPT")
CONFIG_PATH="$SCRIPT_DIR/config.platform"

echo '***'
echo '*** Running hptool'
echo '***'
if [ -e "$CONFIG_PATH" ]; then
  echo "=== sourcing $CONFIG_PATH"
  . "$CONFIG_PATH"
  ok="1"
  if [ -z "$STACK" ]; then echo "STACK is not set"; ok=""; fi
  if [ -z "$CABAL" ]; then echo "CABAL is not set"; ok=""; fi
  if [ -z "$GHC_BINDIST" ]; then echo "GHC_BINDIST is not set"; ok=""; fi
  if [ -z "$ok" ]; then
    echo "config not complete: " $CONFIG_PATH
  else
    echo "*** Using parameters from $CONFIG_PATH"
    exec $HPTOOL "$GHC_BINDIST" "$CABAL" "$STACK" "$@"
  fi
else
  exec $HPTOOL "$@"
fi

