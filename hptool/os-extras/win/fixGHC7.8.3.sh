#!/bin/sh

cat <<EOF
This script will fix up two problems with the GHC 7.8.3 binary
distribution for Windows, 32- and 64-bit.  It will do the following:

1) untar the specified ghc 7.8.3 binary distro
2) in a separate directory, untar the specified ghc 7.8.3 libraries docs
3) copy the libraries/<package>/src to the distro's
   doc/html/libraries/<package>/src
4) remove a specific set of extraneous python files left in the ghc distro
5) create a new bz2 tarball of the modified ghc distro, with "-patched"
   appended to the original name

Parameters:
    ghc-7.8.3-{x86_64,i386}-unknown-mingw32.tar.bz2 libraries.html.tar.bz2

EOF

# ghc-7.8.3-i386-unknown-mingw32.tar.bz2
# ghc-7.8.3-x86_64-unknown-mingw32.tar.bz2

GHC_TARBALL=$1
DOCS_TARBALL=$2

tar_name=${GHC_TARBALL##*/}
tar_vers=${tar_name#*-}
GHC_VERS=${tar_vers%%-*}
tar_root=${tar_name%%.tar*}

# : ${GHC_TARBALL:?"Please supply the path to the ghc-7.8.3 distro tarball"}
# : ${DOCS_TARBALL:?"Please supply the path to the ghc-7.8.3 docs tarball"}

if [ "$GHC_TARBALL" = "" ];
then
    echo "ERROR: Please supply the path to the ghc-7.8.3 distro tarball"
    exit 1
fi

if [ "$DOCS_TARBALL" == "" ];
then
    echo "ERROR: Please supply the path to the ghc-7.8.3 docs tarball"
    exit 1
fi

if ! [ "$GHC_VERS" == "7.8.3" ];
then
    cat<<-EOF
	***
	This script is very specific to GHC 7.8.3.  Using it on any other
	version is not supported.  You will need to examine this script
	and determine if it is doing what you need for other versions.
	***
	EOF
    exit 1
fi


echo '***'
CWD=`pwd`

echo 'Fix missing src/html files...'
mkdir -p fixGHC
cd fixGHC

echo "  tar xf $GHC_TARBALL"
# tar xf $GHC_TARBALL

echo "  tar xf $DOCS_TARBALL"
# tar xf $DOCS_TARBALL

GHC_HTML=ghc-$GHC_VERS/doc/html/libraries

PKGDIRS=`find $GHC_HTML -type d -a -not -name "." -printf "%f\n"`
for lib in $PKGDIRS
do
    # cp -pr libraries/$lib/src $GHC_HTML/$lib
    echo "cp -pr libraries/$lib/src $GHC_HTML/$lib"
done

echo 'Remove extraneous python (and a few other) files...'
list=`cat<<EOF
ghc-$GHC_VERS/mingw/lib/libstdc++.dll.a-gdb.py
ghc-$GHC_VERS/mingw/lib/libgfortran.spec
ghc-$GHC_VERS/mingw/bin/python27.dll
ghc-$GHC_VERS/mingw/bin/libgfortran-3.dll
ghc-$GHC_VERS/mingw/bin/x86_64-w64-mingw32-gfortran.exe
ghc-$GHC_VERS/mingw/bin/gfortran.exe
ghc-$GHC_VERS/mingw/bin/gcj.exe
ghc-$GHC_VERS/mingw/bin/x86_64-w64-mingw32-gcj.exe
EOF`

for f in $list
do
    if [ \( -e $f \) ]
    then
        # rm -f $f
        echo "rm -f $f"
    fi
done

if [ \( -d ghc-$GHC_VERS/mingw/bin/lib \) ]
then
    # rm -rf ghc-$GHC_VERS/mingw/bin/lib
    echo "rm -rf ghc-$GHC_VERS/mingw/bin/lib"
fi

echo "Create $tar_root-patched.tar.bz2..."

# tar cjf $tar_root-patched.tar.bz2 ghc-$GHC_VERS

cd $CWD
