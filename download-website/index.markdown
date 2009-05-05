% Haskell Platform Download <img src="http://code.haskell.org/haskell-platform/download-website/images/beta-icon.png" alt="beta">
%

<img src="http://haskell.org/sitewiki/images/a/a8/Haskell-logo-60.png" alt="Logo">

This is the beta release of the [Haskell Platform], version 2009.2.0.
Find out what ships as part of the platform here: [Haskell: Batteries Included].

The Haskell Platform is a blessed library and tool suite for Haskell.
Taking the best software from [Hackage], it provides a comprehensive,
stable and mature base for Haskell projects to work from. Every system
supporting Haskell needs to ship with the Haskell Platform suite.

Please note that this is a beta release. We do not expect everything to
work perfectly and we would appreciate feedback. Issues related to the
packaging and installers can be filed in the [Platform Bug Tracker].

[Haskell Platform]: http://haskell.org/haskellwiki/Haskell_Platform
[Hackage]: http://hackage.haskell.org
[Platform Bug Tracker]: http://trac.haskell.org/haskell-platform/
[Haskell: Batteries Included]: ./contents.html

Windows
-------

The Windows installer installs GHC 6.10.2, along with the full tool and
library suite,

    haskell-platform-2009.2.0.exe

Mac OS X
--------

The installer for MacOS X is not yet available.  Use the generic unix
source release for now.

Linux
-----

A few distributions already support the Haskell Platform. We expect more to follow soon.

 * Arch Linux
 * Gentoo
 * Nix / NixOS

See your package maintainer for more information. If your distribution is not yet listed then you will need to use the source tarball.

Source
------

For unix systems, there is a generic source installer. You only need GHC
installed to get started:

    haskell-platform-2009.2.0.tar.gz

Download and unpack the installer. Then:

    ./configure --prefix=$HOME
    make
    make install

Note: the source tarball requires that you already have ghc-6.10.x installed.

More info
---------

* [Haskell Platform]
* [Platform Bug Tracker]
* [GHC 6.10.x]

[GHC 6.10.x]: http://haskell.org/ghc
