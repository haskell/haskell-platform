#!/bin/sh

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

[ -e "scripts/config" ] \
  || die "Please run ./configure first"

. scripts/config
. scripts/common.sh

install_pkg () {
  PKG=$1

  cd "packages/${PKG}" 2> /dev/null \
    || die "The directory for the component ${PKG} is missing"

  [ -x Setup ] || die "The ${PKG}/Setup script does not exist or cannot be run"

  ./Setup copy ${VERBOSE} \
    || die "Installing the ${PKG} component failed"

  ./Setup register ${VERBOSE} --gen-pkg-config="${PKG}.conf" \
    || die "Generating the registration information for the package ${PKG} failed"

  if [ -f ${PKG}.conf ]; then
    if test "${USER_INSTALL}" = "YES"; then
      GHC_PKG_DB="--user"
    else
      GHC_PKG_DB="--global"
    fi
    ${GHC_PKG} update ${GHC_PKG_DB} "${PKG}.conf" \
      || die "Registering the package ${PKG} failed"
  fi

  cd ../..
}

# Actually do something!
for pkg in `cat packages/platform.packages`; do
  if is_pkg_installed "${pkg}"; then
    true
  else
    echo "Installing ${pkg}..."
    install_pkg ${pkg}
  fi
done

echo
echo '*******************************************************'
echo '* Installation completed successfully.            '
echo '*'
echo '* Programs installed into:                        '
echo "*   ${prefix}/bin"
echo '*'
echo '* Now do "cabal update" to initialize the package list                          '
echo '*'
echo '* Additional packages may be found at http://hackage.haskell.org'
echo '* or via "cabal list <pattern>"'
echo '*'
echo '* Use "cabal install <foo>" to install additional packages'
echo '*'
echo '*******************************************************'
