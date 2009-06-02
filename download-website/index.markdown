% Haskell Platform Download (Beta)
%

This is the release of the Haskell Platform, version 2009.2.0.1: a
single, standard Haskell distribution for every system.

The Haskell Platform is a blessed library and tool suite for Haskell
distilled from [Hackage], along with installers for a wide variety of
systems.  The contents of the platform are specified here: [Haskell:
Batteries Included].

The platform saves you the task of picking and choosing the best
Haskell libraries and tools to use for a task. Distro maintainers that
support the Haskell Platform can be confident they're fully supporting
Haskell as the developers intend it. Developers targetting the platform
can be confident they have a trusted base of code to work with.

Please note that this is a beta release. We do not expect all the
installers to work perfectly and we would appreciate feedback. Issues
related to the packaging and installers can be filed in the [Platform
Bug Tracker]. Future releases of the platform will include more tools and
libraries to meet developer need.

[Hackage]: http://hackage.haskell.org
[Platform Bug Tracker]: http://trac.haskell.org/haskell-platform/
[Haskell: Batteries Included]: ./contents.html

Windows
-------

The Windows installer provides GHC 6.10.3, along with the full tool and
library suite,

 * [HaskellPlatform-2009.2.0.1-setup.exe]

[HaskellPlatform-2009.2.0.1-setup.exe]: http://hackage.haskell.org/platform/2009.2.0.1/HaskellPlatform-2009.2.0.1-setup.exe

Mac OS X
--------

* The installer for MacOS X is not yet available: use the generic unix source release.*

Linux
-----

*A few distributions already support the Haskell Platform. See your
package maintainer for more information.*

* [Arch Linux]
* Debian
* Fedora
* [Gentoo]
* [NixOS]
* Ubuntu

[Arch Linux]: http://aur.archlinux.org/packages.php?ID=26279
[Gentoo]: http://code.haskell.org/gentoo/gentoo-haskell/dev-haskell/haskell-platform/
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc6102.haskellPlatform/jobstatus

Source
------

For unix systems, there is a generic source installer. You only need GHC
installed to get started:

 * [haskell-platform-2009.2.0.1.tar.gz]

[haskell-platform-2009.2.0.1.tar.gz]: http://hackage.haskell.org/platform/2009.2.0.1/haskell-platform-2009.2.0.1.tar.gz

Download and unpack the installer. Then (possibly with 'sudo'):

    ./configure
    make
    make install

Note: the *source* tarball requires that you already have ghc-6.10.x installed.

Specification
-------------

The platform specification is also available, to aid in constructing
distro packages, in .cabal and tarball form:

 * [haskell-platform.cabal]
 * [haskell-platform cabal package]

[haskell-platform.cabal]: http://hackage.haskell.org/platform/2009.2.0.1/haskell-platform.cabal
[haskell-platform cabal package]: http://hackage.haskell.org/platform/2009.2.0.1/cabal/

Read more
---------

* [Haskell Platform Users]
* [Platform Bug Tracker]
* [GHC 6.10.x]

[Haskell Platform Users]: http://haskell.org/haskellwiki/Haskell_Platform
[GHC 6.10.x]: http://haskell.org/ghc

Tue Jun  2 13:57:40 PDT 2009
