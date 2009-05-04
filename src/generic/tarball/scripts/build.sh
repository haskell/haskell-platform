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

[ -e "scripts/config" ] \
  || die "Please run ./configure first"

. scripts/config

# also check GHC, GHC_PKG
[ -n "$prefix" ] \
  || die "Expected prefix to have been defined in scripts/config"

CABAL_PKG_VER="$(grep Cabal packages/core.packages)"
[ -n "${CABAL_PKG_VER}" ] \
  || die "Expected Cabal as a preinstalled package"
if test "${ALLOW_UNSUPPORTED_GHC}" = "YES"; then
CABAL_PKG_VER="Cabal"
fi

HAPPY_PKG_VER="$(grep happy packages/platform.packages)"
HAPPY_INPLACE="${HAPPY_PKG_VER}/dist/build/happy/happy"
HAPPY_TEMPLATE="${HAPPY_PKG_VER}"

# Initialise the package db
PACKAGE_DB="packages/package.conf.inplace"
[ -e "${PACKAGE_DB}" ] && rm "${PACKAGE_DB}"
echo '[]' > "${PACKAGE_DB}"

# Maybe use a small script instead ? Tested with bash and zsh.
tell() {
  # Save and shift the executable name
  CMD=$1
  shift
  # Build the string of command-line parameters
  PRINT="\"${CMD}\""
  for arg in "$@"; do
      PRINT+=" \"${arg}\""
  done
  # Echo the command
  echo `echo $PRINT`
  # Run the command
  "$CMD" "$@"
}

build_pkg () {
  PKG=$1

  [ -n "${VERBOSE}" ] && echo "Building ${PKG}"

  cd "${PKG}" 2> /dev/null \
    || die "The directory for the component ${PKG} is missing"

  [ -f Setup ] && rm Setup

  tell ${GHC} --make Setup -o Setup -package "${CABAL_PKG_VER}" \
    || die "Compiling the Setup script failed"
  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  if [ -x ../${HAPPY_INPLACE} ]; then
    HAPPY_FLAG1="--with-happy=../${HAPPY_INPLACE}"
    HAPPY_FLAG2="--happy-options=--template=../${HAPPY_TEMPLATE}"
  fi

  tell ./Setup configure --package-db="../../${PACKAGE_DB}" --prefix="${prefix}" \
    --with-compiler=${GHC} --with-hc-pkg=${GHC_PKG} ${HAPPY_FLAG1} ${HAPPY_FLAG2} \
    ${EXTRA_CONFIGURE_OPTS} ${VERBOSE} -O0 \
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
  echo "Building ${pkg}..."
  build_pkg "${pkg}"
done

echo
echo '**************************************************'
echo '* Building each component completed successfully. '
echo '*                                                 '
if test "${USER_INSTALL}" = "YES"; then
echo '* Now do "make install"                           '
else
echo '* Now do "sudo make install"                      '
fi
echo '**************************************************'
