#!/bin/sh

# This script is to in-place modify the existing build/target dir to fix
# two minor GHC 7.8.3 binary distro problems existing in HP2014.2.0.0 RC1
# which we want fixed in RC2 (unless we build the whole tree again for some
# other reason).

# our home
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# These may need to be edited to suit your specific environment
# MSYS_BIN is needed on path for configure scripts;
# PYTHON_BIN is needed on path for python.exe
# NSIS_BIN is needed on path for makensis.exe
MSYS_BIN="/c/Program Files (x86)/MinGW/msys/1.0/bin"
PYTHON_BIN="/c/Program Files/Python27"
NSIS_BIN="/c/Program Files (x86)/NSIS"

BUILDDIR_TO_FIX=$1
GHCLIBRARIES_DOCS=$2

if [ "$BUILDDIR_TO_FIX" = "" ];
then
    echo "ERROR: Please supply the directory path to <buildtofix>"
    exit 1
fi

if [ \! -d $BUILDDIR_TO_FIX/target ]
then
    cat<<-EOF
	ERROR: The first parameter isn't a valid build dir! The 'target'
	directory must be an immediate subdirectory of <buildtofix>
	EOF
    exit 1
fi

if [ "$GHCLIBRARIES_DOCS" = "" ];
then
    echo "ERROR: Please supply the directory path to GHC libraries html docs"
    exit 1
fi

if [ \! \( -d $GHCLIBRARIES_DOCS -o -e $GHCLIBRARIES_DOCS/index.html \) ]
then
    cat<<-EOF
	ERROR: The second parameter isn't a valid directory to the untarred
	GHC 7.8.3 libraries documentation.  It should be the "libraries"
        directory after untarring.
	EOF
    exit 1
fi

echo 'Fix missing src/html files (with a sledge hammer)...'
GHC_HTML=$BUILDDIR_TO_FIX/target/doc/html

rm -rf $GHC_HTML/libraries
(cd $GHC_HTML; cp -pr $GHCLIBRARIES_DOCS .)

# The sledge hammer gave us some extras which aren't on Win, so axe 'em
rm -rf $GHC_HTML/libraries/terminfo-0.4.0.0
rm -rf $GHC_HTML/libraries/unix-2.7.0.1

echo 'Remove extraneous python (and a few other) files... (only on x64)'
list=`cat<<EOF
$BUILDDIR_TO_FIX/target/mingw/lib/libstdc++.dll.a-gdb.py
$BUILDDIR_TO_FIX/target/mingw/lib/libgfortran.spec
$BUILDDIR_TO_FIX/target/mingw/bin/python27.dll
$BUILDDIR_TO_FIX/target/mingw/bin/libgfortran-3.dll
$BUILDDIR_TO_FIX/target/mingw/bin/x86_64-w64-mingw32-gfortran.exe
$BUILDDIR_TO_FIX/target/mingw/bin/gfortran.exe
$BUILDDIR_TO_FIX/target/mingw/bin/gcj.exe
$BUILDDIR_TO_FIX/target/mingw/bin/x86_64-w64-mingw32-gcj.exe
EOF`

for f in $list
do
    if [ \( -e $f \) ]
    then
        echo "rm -f $f"
        rm -f $f
    fi
done

if [ \( -d $BUILDDIR_TO_FIX/target/mingw/bin/lib \) ]
then
    echo "rm -rf $BUILDDIR_TO_FIX/target/mingw/bin/lib"
    rm -rf $BUILDDIR_TO_FIX/target/mingw/bin/lib
fi

# A clean, cruft-free PATH that include python and makensis
export PATH=$MSYS_BIN:$NSIS_BIN:$PYTHON_BIN
which makensis ||
  { echo "Could not find makensis.exe on PATH!"; echo "PATH=$PATH"; exit 1; }
which python ||
  { echo "Could not find python on PATH!"; echo "PATH=$PATH"; exit 1; }

echo 'Re-generate the nsis file lists...'
$DIR/gen_list_files_for_nsis.py $BUILDDIR_TO_FIX/target $BUILDDIR_TO_FIX/installer-parts/inst.dat $BUILDDIR_TO_FIX/installer-parts/uninst.dat

# $BUILDDIR_TO_FIX/installer-parts/uninst.dat
# $BUILDDIR_TO_FIX/installer-parts/inst.dat

echo 'Re-build the installer...'
NSIS_FILE_NAME=Nsisfile.nsi
(cd $BUILDDIR_TO_FIX/installer-parts; makensis $NSIS_FILE_NAME)
