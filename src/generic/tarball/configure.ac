AC_INIT([haskell-platform],[2009.2.0])
AC_CONFIG_AUX_DIR([scripts])
AC_CANONICAL_TARGET

# Sanity check:
AC_CONFIG_SRCDIR([packages/platform.packages])

# Set the output config file:
AC_CONFIG_FILES([scripts/config])

#TODO: have these numbers generated from the .cabal file
REQUIRED_GHC_VER=6.10.2
ACCEPTABLE_GHC_VER=6.10.3

AC_ARG_WITH([ghc],
[AC_HELP_STRING([--with-ghc=ARG],
  [Use ARG as the path to ghc  [default=autodetect]])],
  [GHC="$withval"],
  [if test "$GHC" = ""; then
    AC_PATH_PROG([GHC], [ghc])
  fi])


AC_ARG_WITH([ghc-pkg],
[AC_HELP_STRING([--with-ghc-pkg=ARG],
  [Use ARG as the path to ghc-pkg  [default=autodetect]])],
  [GHC_PKG="$withval"],
  [if test "${GHC_PKG}" = ""; then
    AC_PATH_PROG([GHC_PKG], [ghc-pkg])
  fi])


AC_ARG_ENABLE(unsupported-ghc-version,
[AC_HELP_STRING([--enable-unsupported-ghc-version],
                [Try building using a version of GHC that is not the one]
                [specified by this release of the platform.  This is not]
                [guaranteed to work.])],
  [ALLOW_UNSUPPORTED_GHC=YES],
  [ALLOW_UNSUPPORTED_GHC=NO]
)


AC_MSG_CHECKING([version of ghc])
if GHC_VER=$(${GHC} --numeric-version 2> /dev/null); then
  AC_MSG_RESULT([$GHC_VER])
else
  AC_MSG_RESULT([])
  AC_MSG_ERROR([${GHC} does not seem to work (or maybe it is not actually ghc).])
fi
if test "${ALLOW_UNSUPPORTED_GHC}" = "NO"; then
if test "${GHC_VER}" != "${REQUIRED_GHC_VER}"; then
if test "${GHC_VER}" != "${ACCEPTABLE_GHC_VER}"; then
  AC_MSG_ERROR(
  [The Haskell Platform version ${PACKAGE_VERSION} requires ghc-${REQUIRED_GHC_VER}]
  [It is recommended that you upgrade to ghc-${REQUIRED_GHC_VER} however if you]
  [really want to use this release with an unsupported version of GHC]
  [then use ./configure --enable-unsupported-ghc-version])
fi
fi
fi


AC_MSG_CHECKING([version of ghc-pkg matches])
if GHC_PKG_VER_STR=$(${GHC_PKG} --version); then
  GHC_PKG_VER=$(echo ${GHC_PKG_VER_STR} | cut -d' ' -f 5)
else
  AC_MSG_RESULT([])
  AC_MSG_ERROR([${GHC_PKG} does not seem to work (or maybe it is not actually ghc-pkg).])
fi
if test "${GHC_VER}" = "${GHC_PKG_VER}"; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no (${GHC_PKG_VER})])
  AC_MSG_ERROR(
  [Version mismatch between ${GHC} and ${GHC_PKG}.]
  [If you used --with-ghc= then also use --with-ghc-pkg=])
fi


AC_MSG_CHECKING([ghc actually works])
TRY_COMPILE_GHC([main = putStr "Hello world!\n"],,
  AC_MSG_RESULT([no])
  AC_MSG_ERROR(
  [Your installation of ghc does not appear to work.]
  [It cannot compile a simple program (see config.log for the details).]
  [If you installed ghc from a generic binary tarball then it is worth]
  [checking that you have the 'gmp' C library and header files installed.]
  [(On debian-based systems this package is called libgmp3-dev.)]))
AC_MSG_RESULT([yes])


if test "${ALLOW_UNSUPPORTED_GHC}" = "NO"; then
AC_MSG_CHECKING([the ghc core packages are all installed])
# Cache the list of packages:
echo " $( ${GHC_PKG} list --simple-output ) " > packages/installed.packages

# Is this exact version of the package already installed?
is_pkg_installed () {
  PKG_VER=$1
  grep " ${PKG_VER} " packages/installed.packages > /dev/null 2>&1
}

# Check that we have the core libs installed, which should be distributed
# with ghc
for pkg in $(cat packages/core.packages); do
  if ! is_pkg_installed "$pkg"; then
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([The core package $pkg is missing. It should have been distributed with ${REQUIRED_GHC_VER}])
  fi
done
AC_MSG_RESULT([yes])
fi


AC_ARG_ENABLE([user-install],
[AC_HELP_STRING([--enable-user-install],
  [Register packages into the per-user or the global ghc package database. [default=autodetect]])],
  [case "${enableval}" in
     yes)
       USER_INSTALL=YES
       ;;
     no)
       USER_INSTALL=NO
       ;;
     *)
       AC_MSG_ERROR([The --enable-user-install value should be "yes" or "no"])
       ;;
   esac
  ],
  [if echo "${prefix}" | grep "$HOME"; then
     USER_INSTALL=YES
   else
     USER_INSTALL=NO
   fi
  ])


# Check if we're on OSX or windows
case $target_os in
  darwin*)
    OSX=yes
    ;;
  cygwin32|mingw32)
    WIN32=yes
    ;;
esac


if test "$WIN32" = "yes"; then
  GHC_LIBDIR=`$GHC --print-libdir`
  AC_PATH_PROG(PERL,perl,[notfound],[$GHC_LIBDIR:$PATH])
else
  AC_PATH_PROG(PERL,perl,[notfound])
fi
if test "$PERL" = "notfound"; then
  AC_MSG_ERROR([The perl program is required])
fi


# Check for zlib C library
if test "$WIN32" != "yes"; then
  AC_CHECK_HEADER([zlib.h], [],
    [AC_MSG_ERROR(The zlib C library is required)])
  AC_CHECK_LIB([z], [zlibVersion], [],
    [AC_MSG_ERROR(The zlib C library is required)])
fi


# Check for OpenGL and GLUT
if test "$OSX" != "yes"; then
  AC_CHECK_HEADER([GL/gl.h], [],
    [AC_MSG_ERROR(The OpenGL C library is required)])
  AC_SEARCH_LIBS([glEnd], [GL opengl32], [],
    [AC_MSG_ERROR(The OpenGL C library is required)])

  AC_CHECK_HEADER([GL/glu.h], [],
    [AC_MSG_ERROR(The OpenGL C library is required)])
  AC_SEARCH_LIBS([gluNewQuadric], [GLU glu32], [],
    [AC_MSG_ERROR(The OpenGL C library is required)])

  AC_CHECK_HEADER([GL/glut.h], [],
    [AC_MSG_ERROR(The GLUT C library is required)])
  AC_SEARCH_LIBS([glutMainLoop], [glut glut32], [],
    [AC_MSG_ERROR(The GLUT C library is required)])
fi


AC_SUBST([GHC])
AC_SUBST([GHC_PKG])
AC_SUBST([PERL])

AC_SUBST([ALLOW_UNSUPPORTED_GHC])
AC_SUBST([USER_INSTALL])

AC_OUTPUT
echo '**************************************************'
echo '* Configuration completed successfully.           '
echo '*                                                 '
echo "*   Install prefix: ${prefix}"
echo '*     Haskell packages will be registered into    '
if test "${USER_INSTALL}" = "YES"; then
echo '*     the per-user ghc package database           '
else
echo '*     the global ghc package database             '
fi
echo '*                                                 '
echo '*   If you wish to change these settings then     '
echo '*   use --preifx= and/or --enable-user-install    '
echo '*                                                 '
if test "${USER_INSTALL}" = "YES"; then
echo '* Now do "make" followed by "make install"        '
else
echo '* Now do "make" followed by "sudo make install"   '
fi
echo '**************************************************'
