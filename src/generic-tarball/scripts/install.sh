#!/bin/sh

install_pkg () {
  PKG=$1

  [ -x Setup ] || die "The ${PKG}/Setup script does not exist or cannot be run"

  ./Setup copy ${VERBOSE} \
    || die "Copying the ${PKG} component failed"

  ./Setup register ${VERBOSE} --gen-pkg-config= \
    || die "Registering the ${PKG} package failed"
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
echo "==========================================="
CABAL_BIN="$PREFIX/bin"
if [ -x "$CABAL_BIN/cabal" ]
then
    echo "The 'cabal' program has been installed in $CABAL_BIN/"
    echo "You should either add $CABAL_BIN to your PATH"
    echo "or copy the cabal program to a directory that is on your PATH."
    echo
    echo "The first thing to do is to get the latest list of packages with:"
    echo "  cabal update"
    echo "This will also create a default config file (if it does not already"
    echo "exist) at $HOME/.cabal/config"
    echo
    echo "By default cabal will install programs to $HOME/.cabal/bin"
    echo "If you do not want to add this directory to your PATH then you can"
    echo "change the setting in the config file, for example you could use:"
    echo "symlink-bindir: $HOME/bin"
else
    echo "Sorry, something went wrong."
fi
echo

rm ghc-pkg.list
