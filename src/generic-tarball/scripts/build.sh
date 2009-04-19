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

. ./config.status

# also check GHC, GHC_PKG
[ -n "$PREFIX" ] \
  || die "Expected PREFIX to have been defined in config.status"

CABAL_PKG_VER="$(grep Cabal packages/core.packages)"
[ -n "${CABAL_PKG_VER}" ] \
  || die "Expected Cabal as a preinstalled package"

# Initialise the package db
PACKAGE_DB="packages/package.conf.inplace"
[ -e "${PACKAGE_DB}" ] && rm "${PACKAGE_DB}"
echo '[]' > "${PACKAGE_DB}"

tell() {
  echo $*
  # will this break args with spaces?
  $*
}

build_pkg () {
  PKG=$1

  [ -n "${VERBOSE}" ] && echo "Building ${PKG}"

  cd "${PKG}" 2> /dev/null \
    || die "The directory for the component ${PKG} is missing"

  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  tell ${GHC} --make Setup -o Setup -package "${CABAL_PKG_VER}" \
    || die "Compiling the Setup script failed"
  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  tell ./Setup configure --package-db="../../${PACKAGE_DB}" --prefix="${PREFIX}" \
    --with-compiler=${GHC} --with-hc-pkg=${GHC_PKG} \
    ${EXTRA_CONFIGURE_OPTS} ${VERBOSE} \
    || die "Configuring the ${PKG} package failed"

  tell ./Setup build ${VERBOSE} \
    || die "Building the ${PKG} package failed"

  tell ./Setup register --inplace ${VERBOSE} \
    || die "Registering the ${PKG} package failed"

  cd ..
}

# Actually do something!

cd packages
for pkg in $(cat platform.packages); do
  build_pkg "${pkg}"
done

echo
echo '**************************************************'
echo '* Building each component completed successfully. '
echo '*                                                 '
echo '* Now do "sudo make install"                      '
echo '**************************************************'
