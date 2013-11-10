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
[Fedora]: https://apps.fedoraproject.org/packages/haskell-platform
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

See also: *[justhub]*, for RHEL, CentOS, Scientific Linux, and Fedora support

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[Lucid]: https://launchpad.net/~justinbogner/+archive/haskell-platform
[openSUSE]: https://build.opensuse.org/project/show?project=devel:languages:haskell
[Mandriva]: http://wiki.mandriva.com/en/Development/Tasks/Packaging/Policies/Haskell
[justhub]: http://justhub.org/

----

## Build from source ##

Download the source tarball for Unix-like systems: here

 * ![](icons/source.png)
**<a href="download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2013.2.0.0.tar.gz</a>**  \
<small>SHA-1: `8669bb5add1826c0523fb130c095fb8bf23a30ce`</small>

Get and install GHC 7.6.3 prior to building the platform:

 * [GHC 7.6.3](http://www.haskell.org/ghc/download_ghc_7_6_3)

Finally, unpack the Haskell Platform source tarball, and run (possibly with <tt>sudo</tt>):

        ./configure
        make
        make install

  You may pass the <tt>--prefix</tt> flag to <tt>./configure</tt> to change the default install path.

There is also a [README] file in the tarball with more detailed information on building.

[README]: https://github.com/haskell/haskell-platform/blob/master/src/generic/tarball/README

<!--
**Build from cabal**

If you already have a reasonable Haskell development environment with
GHC 7.0.2 or later and cabal-install, you can build the platform from the Cabal
package alone.

![](icons/cabal.png)
<a href="2010.2.0.0/cabal/haskell-platform-2010.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/cabal'); ">Haskell Platform Cabal Package</a>

To install, unpack the cabal tarball, and run:

        cabal install
 -->

--------

[Prior releases](prior.html)

