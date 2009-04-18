#!/bin/sh

# A script to install the haskell platform 2009.0.0.

# It works by...
# It expects to be run...

# install settings, you can override these by setting environment vars
PREFIX=${PREFIX:-${HOME}/.cabal}
#VERBOSE
#EXTRA_CONFIGURE_OPTS

# programs, you can override these by setting environment vars
GHC=${GHC:-ghc}
GHC_PKG=${GHC_PKG:-ghc-pkg}

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

die "Not implemented further"

# Will we need to install this package, or is a suitable version installed?
need_pkg () {
  PKG=$1
  VER_MATCH=$2
  ! grep " ${PKG}-${VER_MATCH}" ghc-pkg.list > /dev/null 2>&1
}

info_pkg () {
  PKG=$1
  VER=$2
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
  then
    echo "${PKG}-${VER} will be downloaded and installed."
  else
    echo "${PKG} is already installed and the version is ok."
  fi
}

dep_pkg () {
  PKG=$1
  VER_MATCH=$2
  if need_pkg ${PKG} ${VER_MATCH}
  then
    echo
    echo "The Haskell package '${PKG}' is required but it is not installed."
    echo "If you are using a ghc package provided by your operating system"
    echo "then install the corresponding packages for 'parsec' and 'network'."
    echo "If you built ghc from source with only the core libraries then you"
    echo "should install these extra packages. You can get them from hackage."
    die "The Haskell package '${PKG}' is required but it is not installed."
  else
    echo "${PKG} is already installed and the version is ok."
  fi
}

build_pkg () {
  PKG=$1

  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  ${GHC} --make Setup -o Setup \
    || die "Compiling the Setup script failed"
  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  ./Setup configure --user "--prefix=${PREFIX}" \
    --with-compiler=${GHC} --with-hc-pkg=${GHC_PKG} \
    ${EXTRA_CONFIGURE_OPTS} ${VERBOSE} \
    || die "Configuring the ${PKG} package failed"

  ./Setup build ${VERBOSE} \
    || die "Building the ${PKG} package failed"

  ./Setup register --inplace ${VERBOSE} \
    || die "Registering the ${PKG} package failed"
}

do_pkg () {
  PKG=$1
  VER=$2
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
  then
    echo
    echo "Building ${PKG}-${VER}..."
    cd "${PKG}-${VER}"
    install_pkg ${PKG} ${VER}
    cd ..
  fi
}

# Actually do something!

dep_pkg "parsec" "2\."
dep_pkg "network" "[12]\."

info_pkg "Cabal" ${CABAL_VER} ${CABAL_VER_REGEXP}
info_pkg "HTTP"  ${HTTP_VER}  ${HTTP_VER_REGEXP}
info_pkg "zlib"  ${ZLIB_VER}  ${ZLIB_VER_REGEXP}

do_pkg "Cabal" ${CABAL_VER} ${CABAL_VER_REGEXP}
do_pkg "HTTP"  ${HTTP_VER}  ${HTTP_VER_REGEXP}
do_pkg "zlib"  ${ZLIB_VER}  ${ZLIB_VER_REGEXP}

install_pkg "cabal-install"

echo
echo "==========================================="
CABAL_BIN="$PREFIX/bin"
if [ -x "$CABAL_BIN/cabal" ]
then
    echo "The 'cabal' program has been installed in $CABAL_BIN/"
    echo "You should either add $CABAL_BIN to your PATH"
    echo "or copy the cabal program to a directory that is on your PATH."
    echo
    echo "The first thing to do is to get the latest list of packages with:"
    echo "  cabal update"
    echo "This will also create a default config file (if it does not already"
    echo "exist) at $HOME/.cabal/config"
    echo
    echo "By default cabal will install programs to $HOME/.cabal/bin"
    echo "If you do not want to add this directory to your PATH then you can"
    echo "change the setting in the config file, for example you could use:"
    echo "symlink-bindir: $HOME/bin"
else
    echo "Sorry, something went wrong."
fi
echo

rm ghc-pkg.list
