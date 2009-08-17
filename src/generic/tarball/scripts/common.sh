# Just a bunch of sh functions use in build.sh and install.sh

# Maybe use a small script instead ? Tested with bash and zsh.
tell() {
  # Save and shift the executable name
  CMD=$1
  shift
  # Build the string of command-line parameters
  PRINT="\"${CMD}\""
  for arg in "$@"; do
      PRINT="${PRINT} \"${arg}\""
  done
  # Echo the command
  echo `echo $PRINT`
  # Run the command
  "$CMD" "$@"
}

# Is this exact version of the package already installed?
is_pkg_installed () {
  PKG_VER=$1
  grep " ${PKG_VER} " packages/installed.packages > /dev/null 2>&1
}
