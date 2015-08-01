#!/bin/sh

TAR_FILE=$1
tar_name=${TAR_FILE##*/}
tar_vers=${tar_name#*-}
GHC_VERS=${tar_vers%%-*}

# These may need to be edited to suit your specific environment
# MSYS_BIN is needed on path for configure scripts;
# HASK_BIN is needed on path for cabal.exe
# NSIS_BIN is needed on path for makensisw.exe
MSYS_BIN="/c/Program Files (x86)/MinGW/msys/1.0/bin"
HASK_BIN="/c/Program Files (x86)/Haskell/bin"
NSIS_BIN="/c/Program Files (x86)/NSIS"

HPTOOL=hptool/dist/build/hptool/hptool.exe

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

CWD=`pwd`
GHC_BINDIST=build/ghc-bindist/local
MINGW=$GHC_BINDIST/mingw

# A clean, well-lighted, cruft-free PATH
export PATH=$CWD/$GHC_BINDIST/bin:$CWD/$MINGW/bin:$MSYS_BIN:$NSIS_BIN:$HASK_BIN

which cabal ||
  { echo "Could not find cabal.exe on PATH!"; echo "PATH=$PATH"; exit 1; }
which makensisw ||
  { echo "Could not find makensisw.exe on PATH!"; echo "PATH=$PATH"; exit 1; }

echo "> cabal --version"
cabal --version
echo "> which haddock"
which haddock
echo "> haddock --version"
haddock --version

# Make sure makensisw.exe is compiled with support for large strings
#   makensisw="/c/Program\ Files\ \(x86\)/NSIS/Orig/makensis //HDRINFO"
nsis_max_strlen=`makensis //HDRINFO | grep 'NSIS_MAX_STRLEN' | awk '{ match($0, /NSIS_MAX_STRLEN=([0-9]+)/, x); if(x[1] != "") print x[1] }'`
if ! (( nsis_max_strlen >= 8192))
then
    echo '***'
    echo 'Please use the NSIS which has been patched for large strings.'
    echo 'The NSIS found on PATH was built with the following:'
    echo "   NSIS_MAX_STRLEN='$nsis_max_strlen'"
    exit 1
fi

# For now, we are just going to assume a subdirectory in the current
# top-level directory (i.e., where this script is), called "winExternalSrc",
# must exist and must contain glut and GHC user guide files.
#
# Test that assumption.

if [ \! \(    -d winExternalSrc \
           -a -d winExternalSrc/glut \
           -a -d winExternalSrc/glut/include \
           -a -e winExternalSrc/glut/include/glut.h \
           -a -d winExternalSrc/glut/lib \
           -a -d winExternalSrc/glut/lib/i386 \
           -a -e winExternalSrc/glut/lib/i386/libglut32.a \
           -a -e winExternalSrc/glut/lib/i386/glut32.dll \
           -a -d winExternalSrc/glut/lib/x86_64 \
           -a -e winExternalSrc/glut/lib/x86_64/libglut32.a \
           -a -e winExternalSrc/glut/lib/x86_64/glut32.dll \
           -a -d winExternalSrc/doc \
           -a -e winExternalSrc/doc/users_guide.ps \
           -a -e winExternalSrc/doc/users_guide.pdf \
           -a -d winExternalSrc/doc/html \
           -a -d winExternalSrc/winghci \
           -a -e winExternalSrc/winghci/winghci.exe \
        \) ]
then
    echo '***'
    echo 'The Haskell Platform for Windows needs some pre-built components'
    echo 'to be provided:'
    echo '    * winghci (can copy from a previous HP release)'
    echo '    * GLUT library & DLL (e.g,. from freeglut-MinGW-2.8.1-1.mp.zip)'
    echo "    * GHC user's guide (matching the GHC in this HP)"
    echo ''
    echo 'Please create a subdirectory in this directory (where this script'
    echo 'is), with the following contents and structure:'
    cat <<EOF
        ./winExternalSrc/
            glut/
                include/
                    glut.h
                    <and any other headers needed for this particular GLUT>
                lib/
                    i386/
                        libglut32.a
                        glut32.dll
                    x86_64/
                        libglut32.a
                        glut32.dll
            doc/
                users_guide.ps
                users_guide.pdf
                html/
                    <untar of users_guide.html.tar.bz2>
            winghci/
                winghci.exe
                <and any other DLL, etc. needed to run this particular winghci>

EOF

    exit 1
fi

echo '***'
echo "*** Running hptool for $GHC_VERS"
echo '***'
# For Windows platforms, do not build the source tarball
$HPTOOL "$@" build-local build-product
