% Haskell Platform for Linux
%

-------------------------------
< [Home]
-------------------------------

[Home]: index.html

**Community-supported versions of the Haskell Platform on Linux and Unix**

These distributions offer the Haskell Platform in their package repositories.
You can easily install the Haskell Platform through your distribution's native
package manager.

---------                           ---------                           ---------
![](icons/ubuntu.png) [Ubuntu]      ![](icons/debian.png) [Debian]      ![](icons/fedora.png) [Fedora]
![](icons/arch.png) [Arch Linux]    ![](icons/gentoo.png) [Gentoo]      ![](icons/nixos.png) [NixOS]
![](icons/openbsd.png) [OpenBSD]    ![](icons/freebsd.png) [FreeBSD]    ![](icons/mint.png) [Mint]
---------                           ---------                           ---------

[Ubuntu]: http://packages.ubuntu.com/haskell-platform
[Debian]: http://packages.debian.org/haskell-platform
[Fedora]: https://admin.fedoraproject.org/community/?package=haskell-platform#package_maintenance
[Arch Linux]: http://www.archlinux.org/packages/extra/i686/haskell-platform/
[Gentoo]: http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPlatform
[OpenBSD]: http://openports.se/meta/haskell-platform
[FreeBSD]: http://www.freshports.org/devel/hs-haskell-platform/
[Mint]: http://community.linuxmint.com/software/view/haskell-platform

**Information for other systems**

---------                             ---------
![](icons/opensuse.png) [openSUSE]    ![](icons/mandriva.png) [Mandriva]
---------                             ---------

See also: *[justhub]*, for REHL, CentOS, Scientific Linux, and Fedora spport

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[Lucid]: https://launchpad.net/~justinbogner/+archive/haskell-platform
[openSUSE]: https://build.opensuse.org/project/show?project=devel:languages:haskell
[Mandriva]: http://wiki.mandriva.com/en/Development/Tasks/Packaging/Policies/Haskell
[justhub]: http://www.justhub.org/

----

## Build from source ##

Download the source tarball for Unix-like systems: here

 * ![](icons/source.png)
**<a href="http://lambda.haskell.org/platform/download/2012.2.0.0/haskell-platform-2012.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2012.2.0.0.tar.gz</a>**

Get and install GHC 7.4.1 prior to building the platform:

 * [GHC 7.4.1](http://haskell.org/ghc/download_ghc_7_4_1.html)

Finally, unpack the Haskell Platform source tarball, and run (possibly with 'sudo'):

        ./configure
        make
        make install

  You may pass <tt>--prefix</tt> flag to <tt>./configure</tt> to change the default install path.

Complete [instructions for installing from source] are available.

[instructions for installing from source]: http://www.vex.net/~trebla/haskell/haskell-platform.xhtml

<!--
**Build from cabal**

If you already have a reasonable Haskell development environment with
GHC 7.0.2 and cabal-install, you can build the platform from the Cabal
package alone.

![](icons/cabal.png)
<a href="2010.2.0.0/cabal/haskell-platform-2010.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/cabal'); ">Haskell Platform Cabal Package</a>

To install, unpack the cabal tarball, and run:

        cabal install
 -->

--------

[Prior releases](prior.html)

