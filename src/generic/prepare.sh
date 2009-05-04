#!/bin/sh

TOP=../..

PLATFORM_VERSION=$(grep '^version:' ${TOP}/haskell-platform.cabal | sed -e 's/version://' -e 's/ //g')
IMAGE_DIR="haskell-platform-${PLATFORM_VERSION}"

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

which cabal > /dev/null 2>&1 || die "The prepare script needs the cabal program"

echo "Preparing a tarball for ${IMAGE_DIR}"

rm -rf "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/packages"
mkdir "${IMAGE_DIR}/scripts"

runhaskell Build.hs ../../haskell-platform.cabal "${IMAGE_DIR}/packages" \
    || die "Build.hs failed"

PLATFORM_PACKAGE_ID="haskell-platform-${PLATFORM_VERSION}"
echo ${PLATFORM_PACKAGE_ID} >> "${IMAGE_DIR}/packages/platform.packages"
mkdir "${IMAGE_DIR}/packages/${PLATFORM_PACKAGE_ID}"
cp "${TOP}/haskell-platform.cabal" "${TOP}/Setup.hs" "${TOP}/LICENSE" \
  "${IMAGE_DIR}/packages/${PLATFORM_PACKAGE_ID}"

cp tarball/packages/core.packages     "${IMAGE_DIR}/packages/"
cp tarball/scripts/*.sh               "${IMAGE_DIR}/scripts/"
cp tarball/scripts/config.*           "${IMAGE_DIR}/scripts/"
cp tarball/configure.ac tarball/aclocal.m4 tarball/Makefile "${IMAGE_DIR}/"
chmod +x "${IMAGE_DIR}/scripts/"*.sh
chmod +x "${IMAGE_DIR}/scripts/config.guess" "${IMAGE_DIR}/scripts/config.sub"

echo "Running autoreconf"
cd "${IMAGE_DIR}/" && autoreconf && cd ..

tar -czf "${IMAGE_DIR}.tar.gz" "${IMAGE_DIR}"
echo "Created ${IMAGE_DIR}.tar.gz"
