% Haskell Platform Download (Beta)
%

This is the the Haskell Platform, version 2009.2.0.2: a single, standard
Haskell distribution for every system.

The Haskell Platform is a blessed library and tool suite for Haskell
distilled from [Hackage], along with installers for a wide variety of
machines. The contents of the platform is specified here: [Haskell:
Batteries Included].

The platform saves you the task of picking and choosing the best Haskell
libraries and tools to use for a task. Distro maintainers that support
the Haskell Platform can be confident they're fully supporting Haskell
as the developers intend it. Developers targetting the platform can be
confident they have a trusted base of code to work with.

Please note that this is a beta release - we would appreciate feedback.
Issues related to the packaging and installers can be filed in the
[Platform Bug Tracker]. Future releases of the platform will include
more tools and libraries to meet developer need.

[Hackage]: http://hackage.haskell.org
[Platform Bug Tracker]: http://trac.haskell.org/haskell-platform/
[Haskell: Batteries Included]: ./contents.html

Windows
-------

The Windows installer provides GHC 6.10.4, along with the full tool and
library suite,

 * [HaskellPlatform-2009.2.0.2-setup.exe]

[HaskellPlatform-2009.2.0.2-setup.exe]: http://hackage.haskell.org/platform/2009.2.0.2/HaskellPlatform-2009.2.0.2-setup.exe

Mac OS X
--------

The MacOS X installer provides GHC 6.10.4 and the full tool and library suite:

 * [haskell-platform-2009.2.0.2-i386.dmg] (Leopard/x86)

[haskell-platform-2009.2.0.2-i386.dmg]: http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2-i386.dmg

Linux
-----

*A few distributions already support the Haskell Platform. See your
package maintainer for more information.*

* [Arch Linux]
* [Gentoo]
* [NixOS]
* Debian ([Debian Status])
* Ubuntu
* Fedora

[Arch Linux]: http://aur.archlinux.org/packages.php?ID=26279
[Gentoo]: http://code.haskell.org/gentoo/gentoo-haskell/dev-haskell/haskell-platform/
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc6102.haskellPlatform/jobstatus
[Debian Status]: http://wiki.debian.org/Haskell/Platform

Source
------

For unix systems, there is a generic source installer. You only need GHC
installed to get started:

 * [haskell-platform-2009.2.0.2.tar.gz]

[haskell-platform-2009.2.0.2.tar.gz]: http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2.tar.gz

Download and unpack the installer. Then (possibly with 'sudo'):

    ./configure
    make
    make install

Note: the *source* tarball requires that you already have ghc installed.

Specification
-------------

The platform specification is also available, to aid in constructing
distro packages, in .cabal and tarball form:

 * [haskell-platform.cabal]
 * [haskell-platform cabal package]

[haskell-platform.cabal]: http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform.cabal
[haskell-platform cabal package]: http://hackage.haskell.org/platform/2009.2.0.2/cabal/

Older Releases
--------------

 * [2009.2.0.1]
 * [2009.2.0]

[2009.2.0.1]: http://hackage.haskell.org/platform/2009.2.0.1/
[2009.2.0]: http://hackage.haskell.org/platform/2009.2.0/

Read more
---------

* [Platform Bug Tracker]
* [GHC 6.10.x]

[GHC 6.10.x]: http://haskell.org/ghc

Fri Jul 31 15:11:11 PDT 2009
