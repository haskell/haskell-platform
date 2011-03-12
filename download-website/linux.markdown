% Haskell Platform for Linux
%

-------------------------------
< [Home]
-------------------------------

[Home]: index.html

**Community-supported versions of the Haskell Platform on Linux**

---------                                                               ---------                                                               ---------
![](http://hackage.haskell.org/platform/icons/ubuntu.png) [Ubuntu]      ![](http://hackage.haskell.org/platform/icons/debian.png) [Debian]      ![](http://hackage.haskell.org/platform/icons/fedora.png) [Fedora]
![](http://hackage.haskell.org/platform/icons/arch.png) [Arch Linux]    ![](http://hackage.haskell.org/platform/icons/gentoo.png) [Gentoo]      ![](http://hackage.haskell.org/platform/icons/nixos.png) [NixOS]
---------                                                               ---------                                                               ---------

[Ubuntu]: http://packages.ubuntu.com/haskell-platform
[Debian]: http://packages.debian.org/haskell-platform
[Fedora]: https://admin.fedoraproject.org/community/?package=haskell-platform#package_maintenance
[Arch Linux]: http://www.archlinux.org/packages/extra/i686/haskell-platform/
[Gentoo]: http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc702.haskellPlatform_2011_2_0_0/

**Information for other systems**

---------                                                                 ---------                                                                  ---------
![](http://hackage.haskell.org/platform/icons/opensuse.png) [openSUSE]    ![](http://hackage.haskell.org/platform/icons/mandriva.png) [Mandriva]     ![](http://hackage.haskell.org/platform/icons/freebsd.png) [FreeBSD]
---------                                                                 ---------                                                                  ---------

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[Lucid]: https://launchpad.net/~justinbogner/+archive/haskell-platform
[openSUSE]: https://build.opensuse.org/project/show?project=devel:languages:haskell
[Mandriva]: http://wiki.mandriva.com/en/Development/Tasks/Packaging/Policies/Haskell
[FreeBSD]: http://wiki.freebsd.org/Haskell

**Build from source**

For Unix systems (including Mac OS X), there is a source installer.

![](http://hackage.haskell.org/platform/icons/source.png)
<a href="http://lambda.galois.com/hp-tmp/2011.2.0.0/haskell-platform-2011.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2011.2.0.0.tar.gz</a>

You need GHC 7.0.2 installed before building the platform. You can get this from your distro or
you can get a [GHC 7.0.2 generic binary].

[GHC 7.0.2 generic binary]: http://haskell.org/ghc/download_ghc_7_0_2.html#distros

To install from source download and unpack the installer, then (possibly with 'sudo'):

        ./configure
        make
        make install

You may pass --prefix flags to change the default install path.

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

**Older Releases**

Older releases of the Haskell Platform are available:

* <a id="download" href="http://hackage.haskell.org/platform/2010.2.0.0/haskell-platform-2010.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2010.2.0.0</a> source, July 2010.
* <a id="download" href="http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2010.1.0.0</a> source, March 2010.
* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2009.2.0.2</a> source, July 2009.
* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0.1/haskell-platform-2009.2.0.1.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2009.2.0.1</a> source, June 2009.
* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0/haskell-platform-2009.2.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2009.2.0</a> source, May 2009.

