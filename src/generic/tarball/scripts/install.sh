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

install_pkg () {
  PKG=$1

  [ -x Setup ] || die "The ${PKG}/Setup script does not exist or cannot be run"

  ./Setup copy ${VERBOSE} \
    || die "Installing the ${PKG} component failed"

  ./Setup register ${VERBOSE} --gen-pkg-config="${PKG}.conf" \
    || die "Generating the registration information for the package ${PKG} failed"

  if [ -f ${PKG}.conf ]; then
    ${GHC_PKG} update --global "${PKG}.conf" \
      || die "Registering the package ${PKG} failed"
  fi
}

# Actually do something!
cd packages
for pkg in $(cat platform.packages); do
  cd "${pkg}" || die "The directory for the component ${PKG} is missing"
  echo "Installing ${pkg}..."
  install_pkg ${pkg}
  cd ..
done

echo
echo '**************************************************'
echo '* Installation completed successfully.            '
echo '*                                                 '
echo '* Programs installed into:                        '
echo "*   ${prefix}/bin                                 "
echo '*                                                 '
echo '* Now do "cabal update"                           '
echo '**************************************************'
