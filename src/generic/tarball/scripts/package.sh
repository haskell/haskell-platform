#!/bin/sh

ROOT_DIR=$1 # where to assemble a copy of the eventual install tree
CONF_DIR=$2 # where to place package .conf files

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

test "$#" -eq 2 \
  || die "Needs to args: $0 <root_dir> <conf_dir>"

[ -d "${ROOT_DIR}" ] \
  || die "root_dir does not exist: ${ROOT_DIR}"
[ -d "${CONF_DIR}" ] \
  || die "conf_dir does not exist: ${CONF_DIR}"

package_pkg () {
  PKG=$1

  cd "packages/${PKG}" 2> /dev/null \
    || die "The directory for the component ${PKG} is missing"

  [ -x Setup ] || die "The ${PKG}/Setup script does not exist or cannot be run"

  ./Setup copy ${VERBOSE} --destdir="${ROOT_DIR}" \
    || die "Copying the ${PKG} component failed"

  ./Setup register ${VERBOSE} --gen-pkg-config="${CONF_DIR}/${PKG}.conf" \
    || die "Generating the registration information for the package ${PKG} failed"

  cd ../..
}

# Actually do something!
for pkg in `cat packages/platform.packages`; do
  if is_pkg_installed "${pkg}"; then
    true
  else
    echo "Packaging ${pkg}..."
    package_pkg ${pkg}
  fi
done

echo
echo '*******************************************************'
echo '* Packaging completed successfully.            '
echo '*'
echo '* Installation tree copied into:                        '
echo "*   ${ROOT_DIR}"
echo '* Package registration files placed in:                        '
echo "*   ${CONF_DIR}"
echo '*'
echo '*******************************************************'
