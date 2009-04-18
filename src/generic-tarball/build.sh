#!/bin/sh

# A script to build the haskell platform 2009.0.0.

# It works by...
# It expects to be run...

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

[ -e "config.status" ] \
  || die "Please run ./configure first"

source "config.status"

# also check GHC, GHC_PKG
[ -n "$PREFIX" ] \
  || die "Expected PREFIX to have been defined in config.status"

CABAL_PKG_VER="$(grep Cabal core.packages)"
[ -n "${CABAL_PKG_VER}" ] \
  || die "Expected Cabal as a preinstalled package"

PACKAGE_DB="inplace-packagedb.conf"
[ -e "${PACKAGE_DB}" ] && rm "${PACKAGE_DB}"
echo '[]' > "${PACKAGE_DB}"

# Will we need to install this package, or is a suitable version installed?
need_pkg () {
  PKG_VER=$1
  ! grep " ${PKG_VER} " installed.packages > /dev/null 2>&1
}

tell() {
  echo $*
  # will this break args with spaces?
  $*
}

build_pkg () {
  PKG=$1

  [ -n "${VERBOSE}" ] && echo Building ${PKG}

  pushd "${PKG}" > /dev/null 2>&1
  
  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  tell ${GHC} --make Setup -o Setup -package "${CABAL_PKG_VER}" \
    || die "Compiling the Setup script failed"
  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  tell ./Setup configure --package-db="../${PACKAGE_DB}" --prefix="${PREFIX}" \
    --with-compiler=${GHC} --with-hc-pkg=${GHC_PKG} \
    ${EXTRA_CONFIGURE_OPTS} ${VERBOSE} \
    || die "Configuring the ${PKG} package failed"

  tell ./Setup build ${VERBOSE} \
    || die "Building the ${PKG} package failed"

  tell ./Setup register --inplace ${VERBOSE} \
    || die "Registering the ${PKG} package failed"

  popd > /dev/null 2>&1
}

# Actually do something!

# Cache the list of packages:
echo "Checking installed packages..."
echo " $( ${GHC_PKG} list --simple-output ) " > installed.packages

for p in $(cat platform.packages); do
  if need_pkg "$p"; then
    build_pkg "$p"
  else
    echo "Found pre-installed $p"
  fi
done

die "Not implemented further"

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

rm installed.packages
