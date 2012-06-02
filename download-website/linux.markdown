% Haskell Platform for Linux
%

-------------------------------
< [Home]
-------------------------------

[Home]: index.html

**Community-supported versions of the Haskell Platform on Linux and Unix**

---------                                                               ---------                                                               ---------
![](http://hackage.haskell.org/platform/icons/ubuntu.png) [Ubuntu]      ![](http://hackage.haskell.org/platform/icons/debian.png) [Debian]      ![](http://hackage.haskell.org/platform/icons/fedora.png) [Fedora]
![](http://hackage.haskell.org/platform/icons/arch.png) [Arch Linux]    ![](http://hackage.haskell.org/platform/icons/gentoo.png) [Gentoo]      ![](http://hackage.haskell.org/platform/icons/nixos.png) [NixOS]
![](http://hackage.haskell.org/platform/icons/openbsd.png) [OpenBSD]    ![](http://hackage.haskell.org/platform/icons/freebsd.png) [FreeBSD]    ![](http://hackage.haskell.org/platform/icons/mint.png) [Mint]
---------                                                               ---------                                                               ---------

[Ubuntu]: http://packages.ubuntu.com/haskell-platform
[Debian]: http://packages.debian.org/haskell-platform
[Fedora]: https://admin.fedoraproject.org/community/?package=haskell-platform#package_maintenance
[Arch Linux]: http://www.archlinux.org/packages/extra/i686/haskell-platform/
[Gentoo]: http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc741.haskellPlatform
[OpenBSD]: http://openports.se/meta/haskell-platform
[FreeBSD]: http://www.freshports.org/devel/hs-haskell-platform/
[Mint]: http://community.linuxmint.com/software/view/haskell-platform

**Information for other systems**

---------                                                                 ---------                                                                  ---------
![](http://hackage.haskell.org/platform/icons/opensuse.png) [openSUSE]    ![](http://hackage.haskell.org/platform/icons/mandriva.png) [Mandriva]
---------                                                                 ---------                                                                  ---------

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[Lucid]: https://launchpad.net/~justinbogner/+archive/haskell-platform
[openSUSE]: https://build.opensuse.org/project/show?project=devel:languages:haskell
[Mandriva]: http://wiki.mandriva.com/en/Development/Tasks/Packaging/Policies/Haskell

----

## Build from source ##

Download the source tarball for Unix-like systems: here

 * ![](http://hackage.haskell.org/platform/icons/source.png)
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

![](http://hackage.haskell.org/platform/icons/cabal.png)
<a href="http://hackage.haskell.org/platform/2010.2.0.0/cabal/haskell-platform-2010.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/cabal'); ">Haskell Platform Cabal Package</a>

To install, unpack the cabal tarball, and run:

        cabal install
 -->

--------

[Prior releases](prior.html)

