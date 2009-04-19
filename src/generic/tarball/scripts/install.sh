#!/bin/sh

install_pkg () {
  PKG=$1

  [ -x Setup ] || die "The ${PKG}/Setup script does not exist or cannot be run"

  ./Setup copy ${VERBOSE} \
    || die "Copying the ${PKG} component failed"

  ./Setup register ${VERBOSE} --gen-pkg-config=${PKG}.pkg \
    || die "Generating the registration information for the package ${PKG} failed"

  ${GHC_PKG} update --global
    || die "Registering the package ${PKG} failed"
}

# Actually do something!
cd packages
for pkg in $(cat platform.packages); do
  cd "${pkg}"
  echo "Installing ${pkg}..."
  install_pkg ${pkg}
  cd ..
done

echo
echo '**************************************************'
echo '* Installation completed successfully.            '
echo '*                                                 '
echo '* Programs installed into:                        '
echo "*   ${PREFIX}/bin                                 "
echo '*                                                 '
echo '* Now do "cabal update"                           '
echo '**************************************************'
