#!/bin/sh
#
# This is the Haskell Platform installer script for Posix systems.
#
# It expects a tar archive named "hp-usr-local.tar.gz" to be present
# in the current working directory.
#
# It will unpack the archive at / and run the activate-hs script.
#
# This script needs be run as root.

usr_local_tar="./hp-usr-local.tar.gz"

if ! test -f ""$usr_local_tar""; then
  echo Archive $usr_local_tar not found.
  exit 1
fi

if ! test -w /; then
  echo "The directory / is not writable. Please run this script as root."
  exit 1
fi

echo "Unpacking $usr_local_tar to /..."
if ! tar -C / -xf "$usr_local_tar"; then
  echo "Unpack failed - aborting installation."
  exit 1
fi

echo "Running /usr/local/bin/activate-hs ..."
/usr/local/bin/activate-hs

