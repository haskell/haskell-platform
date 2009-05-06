% Haskell Platform Download (Beta)
%

This is the beta release of the [Haskell Platform], version 2009.2.0.

The Haskell Platform is a blessed library and tool suite for Haskell.
The contents of the platform are specified here: [Haskell: Batteries Included].
The platform saves you the task of picking and choosing the best
libraries to use for a task, and distros that support the Haskell
Platform can be confident they're fully supporting Haskell as the
developers intend it.

The platform takes the best software from [Hackage], providing a
comprehensive, stable and mature base for Haskell projects to work from.
Every system supporting Haskell needs to ship with the Haskell Platform suite.

Please note that this is a beta release. We do not expect all the
installers to work perfectly and we would appreciate feedback. Issues
related to the packaging and installers can be filed in the [Platform
Bug Tracker].

[Haskell Platform]: http://haskell.org/haskellwiki/Haskell_Platform
[Hackage]: http://hackage.haskell.org
[Platform Bug Tracker]: http://trac.haskell.org/haskell-platform/
[Haskell: Batteries Included]: ./contents.html

Windows
-------

The Windows installer provides GHC 6.10.2, along with the full tool and
library suite,

 * [HaskellPlatform-2009.2.0-setup.exe]

[HaskellPlatform-2009.2.0-setup.exe]: http://haskell.org/download/platform/2009.2.0/HaskellPlatform-2009.2.0-setup.exe

Mac OS X
--------

*The installer for MacOS X is not yet available: use the generic unix source release. *

Linux
-----

*A few distributions already support the Haskell Platform. See your
package maintainer for more information.*

* [Arch Linux]
* [NixOS]

[Arch Linux]: http://aur.archlinux.org/packages.php?ID=26279
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc6102.haskellPlatform/jobstatus

Source
------

For unix systems, there is a generic source installer. You only need GHC
installed to get started:

 * [haskell-platform-2009.2.0.tar.gz]

[haskell-platform-2009.2.0.tar.gz]: http://haskell.org/download/platform/2009.2.0/haskell-platform-2009.2.0.tar.gz

Download and unpack the installer. Then (possibly with 'sudo'):

    ./configure
    make
    make install

Note: the source tarball requires that you already have ghc-6.10.x installed.

Read more
---------

* [Haskell Platform]
* [Platform Bug Tracker]
* [GHC 6.10.x]

[GHC 6.10.x]: http://haskell.org/ghc
