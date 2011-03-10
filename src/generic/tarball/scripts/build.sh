#!/bin/sh

# A script to build the haskell platform.

# It works by...
# It expects to be run...

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

[ -f "scripts/config" ] \
  || die "Please run ./configure first"

. scripts/config
. scripts/common.sh

# also check GHC, GHC_PKG
[ -n "$prefix" ] \
  || die "Expected prefix to have been defined in scripts/config"

CABAL_PKG_VER="`grep Cabal packages/core.packages`"
[ -n "${CABAL_PKG_VER}" ] \
  || die "Expected Cabal as a preinstalled package"
if test "${ALLOW_UNSUPPORTED_GHC}" = "YES"; then
CABAL_PKG_VER="Cabal"
fi

HAPPY_PKG_VER="`grep happy packages/platform.packages`"
HAPPY_INPLACE="${HAPPY_PKG_VER}/dist/build/happy/happy"
HAPPY_TEMPLATE="${HAPPY_PKG_VER}"

ALEX_PKG_VER="`grep alex packages/platform.packages`"
ALEX_INPLACE="${ALEX_PKG_VER}/dist/build/alex/alex"

CABAL_INSTALL_PKG_VER="`grep cabal-install packages/platform.packages`"
CABAL_INSTALL_INPLACE="${CABAL_INSTALL_PKG_VER}/dist/build/cabal/cabal"

# Creates dependency on hscolour
# HADDOCK_FLAG="--hyperlink-source"

# Initialise the package db
PACKAGE_DB="packages/package.conf.inplace"
[ -f "${PACKAGE_DB}" ] && rm "${PACKAGE_DB}"
echo '[]' > "${PACKAGE_DB}"

build_pkg () {
  PKG=$1
  NAME=${PKG%-*}

  cd "packages/${PKG}" 2> /dev/null \
    || die "The directory for the component ${PKG} is missing"

  [ -f Setup ] && rm Setup

  tell ${GHC} --make Setup -o Setup -package "${CABAL_PKG_VER}" \
    || die "Compiling the Setup script failed"
  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  if [ -x ../${HAPPY_INPLACE} ]; then
    HAPPY_FLAG1="--with-happy=../${HAPPY_INPLACE}"
    HAPPY_FLAG2="--happy-options=--template=../${HAPPY_TEMPLATE}"
  fi
  if [ -x ../${ALEX_INPLACE} ]; then
    ALEX_FLAG="--with-alex=../${ALEX_INPLACE}"
  fi
  if [ -x ../${CABAL_INSTALL_INPLACE} ] \
     && echo ${PKG} | grep 'haskell-platform' > /dev/null 2>&1; then
    CABAL_INSTALL_FLAG="--with-cabal-install=../${CABAL_INSTALL_INPLACE}"
  fi
  if test "${ENABLE_PROFILING}" = "YES"; then
    CABAL_PROFILING_FLAG="--enable-library-profiling"
  fi

  # Work around for Cabal 1.8.0.2 not registering properly
  GHC_PKG_FLAG=--ghc-pkg-option=--package-conf="../../${PACKAGE_DB}" 

  # Include the user package database if ${USER_INSTALL}
  if test "${USER_INSTALL}" = "YES"; then
      USER_PKG_FLAG=--user
  else
      USER_PKG_FLAG=
  fi

  tell ./Setup configure --package-db="../../${PACKAGE_DB}" --prefix="${prefix}" \
    --with-compiler=${GHC} --with-hc-pkg=${GHC_PKG} --with-hsc2hs=${HSC2HS} \
    ${HAPPY_FLAG1} ${HAPPY_FLAG2} ${ALEX_FLAG} \
    ${CABAL_INSTALL_FLAG} ${CABAL_PROFILING_FLAG} \
    ${EXTRA_CONFIGURE_OPTS} ${VERBOSE} ${GHC_PKG_FLAG} ${USER_PKG_FLAG} \
    || die "Configuring the ${PKG} package failed"

  tell ./Setup build ${VERBOSE} \
    || die "Building the ${PKG} package failed"

  tell ./Setup register --inplace ${VERBOSE} \
    || die "Registering the ${PKG} package failed"

  if test "${NAME}" \!= "haskell-platform"; then
    tell ./Setup haddock ${VERBOSE} ${HADDOCK_FLAG} \
      || echo "Generating the ${PKG} package documentation failed"
  fi
  cd ../..
}

# Let them know what we're doing 
echo '**************************************************'
echo "Scanning system for any installed Haskell Platform components..."
already_installed=""
will_install=""

# Partition the platform packages into those already found, and those
# we'll need to install.
for pkg in `cat packages/platform.packages`; do
  if is_pkg_installed "${pkg}"; then
    already_installed="${already_installed} ${pkg}"
  else
    will_install="${will_install} ${pkg}"
  fi
done

# State what we found.
echo
echo -n "Found:"

if test -z "${already_installed}" ; then
echo "None."
else
echo "${already_installed}"
fi

echo
echo -n "New packages to install: "

if test -z "${will_installed}" ; then
echo "None! All done."
else
echo "${will_install}"
fi
echo

# Actually do something!
for pkg in `cat packages/platform.packages`; do
  if is_pkg_installed "${pkg}"; then
    true
  else
    echo '**************************************************'
    echo "Building ${pkg}"
    build_pkg "${pkg}"
  fi
done

echo
echo '**************************************************'
echo '* Building Haskell Platform completed successfully. '
echo '*                                                 '
if test "${USER_INSTALL}" = "YES"; then
echo '* Now do "make install"                           '
else
echo '* Now do "sudo make install"                      '
fi
echo '**************************************************'
