#!/bin/sh

. tarball/scripts/versions

IMAGE_DIR="haskell-platform-${PLATFORM_VERSION}"

echo "Preparing a tarball for ${IMAGE_DIR}"

rm -rf "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/"
mkdir "${IMAGE_DIR}/packages"
mkdir "${IMAGE_DIR}/scripts"

runghc Build.hs ../../haskell-platform.cabal "${IMAGE_DIR}/packages"

cp tarball/packages/core.packages        "${IMAGE_DIR}/packages/"
cp -p tarball/scripts/*.sh               "${IMAGE_DIR}/scripts/"
cp -p tarball/configure tarball/Makefile "${IMAGE_DIR}/"

tar -c "${IMAGE_DIR}" -zf "${IMAGE_DIR}.tar.gz"
echo "Created ${IMAGE_DIR}.tar.gz"
