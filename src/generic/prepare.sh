#!/bin/sh

. tarball/scripts/versions

IMAGE_DIR="haskell-platform-${PLATFORM_VERSION}"

die () {
  echo
  echo "Error:"
  echo $1 >&2
  exit 2
}

which cabal 2> /dev/null || die "The prepare script needs the cabal program"

echo "Preparing a tarball for ${IMAGE_DIR}"

rm -rf "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/packages"
mkdir "${IMAGE_DIR}/scripts"

runghc Build.hs ../../haskell-platform.cabal "${IMAGE_DIR}/packages" \
    || die "Build.hs failed"

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
