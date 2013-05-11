#!/bin/sh

TOP=../..
HP_CABAL=${TOP}/haskell-platform.cabal

PLATFORM_VERSION=$(grep '^version:' ${HP_CABAL} | sed -e 's/version://' -e 's/ //g')
IMAGE_DIR="haskell-platform-${PLATFORM_VERSION}"

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

packageIDs () {
  sed -n -e "/begin $1/,/end $1/p" "${HP_CABAL}" \
    | grep '==' \
    | sed -e 's/^[ -]*\([^ ]*\) *==\([0-9.]*\).*/\1-\2/'
}

which cabal > /dev/null 2>&1 || die "The prepare script needs the cabal program"

echo "Preparing a tarball for ${IMAGE_DIR}"

rm -rf "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/packages"
mkdir "${IMAGE_DIR}/scripts"

SRC_PKGS=$(packageIDs platform)
for pkg in ${SRC_PKGS}
do
    (cd "${IMAGE_DIR}/packages" && cabal unpack $pkg)
done

cabal install --dry-run --reinstall ${SRC_PKGS} \
    | tail -n +3 | cut -d ' ' -f 1 > "${IMAGE_DIR}/packages/platform.packages"


PLATFORM_PACKAGE_ID="haskell-platform-${PLATFORM_VERSION}"
echo ${PLATFORM_PACKAGE_ID} >> "${IMAGE_DIR}/packages/platform.packages"
HP_PKG_DIR="${IMAGE_DIR}/packages/${PLATFORM_PACKAGE_ID}"
mkdir "${HP_PKG_DIR}"
cp "${HP_CABAL}" "${TOP}/Setup.hs" "${TOP}/LICENSE" "${HP_PKG_DIR}"

packageIDs core > "${IMAGE_DIR}/packages/core.packages"

cp tarball/scripts/*.sh               "${IMAGE_DIR}/scripts/"
cp tarball/scripts/config.*           "${IMAGE_DIR}/scripts/"
cp tarball/configure.ac tarball/aclocal.m4 tarball/Makefile tarball/README \
                                      "${IMAGE_DIR}/"
chmod +x "${IMAGE_DIR}/scripts/"*.sh
chmod +x "${IMAGE_DIR}/scripts/config.guess" "${IMAGE_DIR}/scripts/config.sub"

echo "Running autoreconf"
cd "${IMAGE_DIR}/" && autoreconf && cd ..

tar -czf "${IMAGE_DIR}.tar.gz" "${IMAGE_DIR}"
echo "Created ${IMAGE_DIR}.tar.gz"
