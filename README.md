**THIS PROJECT IS NOW DEPRECATED **
-------------

The Haskell Platform is deprecated and no longer the recommended way
to install Haskell.  The recommended way is to follow the instructions on
[the Haskell.org Downloads page](https://www.haskell.org/downloads/).


Haskell Platform
----------------

OVERVIEW
--------
"Haskell Platform" is a combination of the GHC compiler and core libraries,
plus additional tools and libraries covering a range of common programming
tasks. The platform is maintained and released so as to be a consistent, stable
base. It aims to be a quick way to a working Haskell environment, and a solid
foundation on which to base production software.

While end-users _can_ build the platform themselves, it is generally recommended,
if possible, to use the prebuilt binaries available from https://www.haskell.org/platform/

The platform distribution and tooling are more intended instead for those wishing to package a platform installer for use by others.

DISTRIBUTION
------------
Again: If you want to install the haskell platform, this is NOT the right location. You can download prebuilt installers for most systems from

https://www.haskell.org/platform/

From there you can get the following:

*installer* - For Windows and OS X, the platform is distributed as a standard
installer for the operating system. It contains a fully built version of the
platform, accompanying documentation, and additional scripts and files needed
to integrate well with the standard environment and development tools.

Running the installer is all you need to get a working Haskell environment.

*OS distribution packages* - For many Linux, BSD, and other similar Posix,
the platform has been packaged into packages for the local package manager.
These can be selected and installed, from the appropriate package repos, with
the standard package manager tools.

*generic bindist* - For Linux, there is a generic bindist. This is compiled
against standard versions of the common system libraries, and if your system
has those, you can generally just unpack this tarball, and run a script to
get it setup.

DISTRIBUTION OF TOOLKIT FOR BUILDING INSTALLERS
------------

If you really want to get the toolkit to build your _own_ platform installer, then you're in the right spot.

The platform installer toolkit comes in several forms:

*source tarball* - This is a specially packaged version of the repo that
includes the sources of the packages that make up the platform, and excludes
some of the ancillary things in the source repo. You can use this to build
the platform without access to hackage or even an internet connection. You still
need a GHC bindist, as well as cabal and stack binaries, either built from source or 
available from their respective websites.

This is also available from https://www.haskell.org/platform/

*source repo* - This is the source of the system that builds the platform. It
includes the file that defines the versions of GHC and other packages that make
up the platform. You can use this to build the platform from just a GHC bindist.

You can check out the official release from github:
     http://github.com/haskell/haskell-platform
     
The master branch is always stable, and releases are tagged like "2014.2.0.0".
Development usually happens in other branches.


REQUIREMENTS FOR BUILDING
-------------------------
You need the platform build files, either from the *source repo* or the
*source tarball*.  Build instructions are the same for either.

You need a GHC bindist that matches the OS you are compiling on. It must also
match the GHC version used by the platform, which you can find by looking in the Releases*.hs files.

You can get the bindists from

    https://www.haskell.org/ghc/download

The machine doing the build needs to have a working Haskell setup: Usually,
GHC (7.4 or later), Cabal (1.24 or later), and haddock and HsColour must be on
the $PATH.

You also need a proper version of the cabal and stack binaries to be bundled.

You can build a cabal directly from hackage and get a stack from http://docs.haskellstack.org

BUILDING
--------
The platform is now built by a program called hptool. That tool is a shake based
build system that creates both the traditional haskell-platform source tarball,
and can build a complete, hermetic build of the platform for use in building OS
installer packages.

In either the source repo or unpacked source tarball, simply run this:

   ./platform.sh $PATH_TO_GHC_BINDIST_TARBALL  $PATH_TO_CABAL_EXECUTABLE $PATH_TO_STACK_EXECUTABLE

This will build the hptool itself, and then use that tool to build first the
platform source tarball, and finally the hermetic build of all the platform
packages.

By default this now builds a "minimal" installer that does not install libraries beyond core. To 
build a "full" installer with a broader range of libraries pre-installed in the global store, pass 
a "-f" option.

If you are building for a Posix like system (Linux, or BSD), then you can add
the command line option --prefix to specify where, on the target system the
tree of built things will be placed. It defaults to "/usr/local/haskell".  The
build will include another directory under that named "ghc-x.y.z-arch" and
everything will be installed under there.

Adding -j<n> (no space between the j and the number of cores) to the build invocation
will enable building on multiple cores at once.


INSTALLATION
------------
After the build completes, it will print instructions for how to take the build
product and install it on systems. The build products are in:
  build/product

For OS X and Windows, the built product is just a standard installer. Copy it
to the target system and install it.

For Posix like systems, the built product is a tarball, which should be unpacked
at / (it includes the prefix spec'd in the build). Then, on the target, you
must run the activate-hs script in the installed bin dir, usually:
  /usr/local/haskell/ghc-x.y.z-arch/bin/activate-hs
That script will do the final package registrations, and symlink all the command
line tools (ghc, haddock, etc..) into /usr/local/bin. Run the script with -n or
-? to find out more.

