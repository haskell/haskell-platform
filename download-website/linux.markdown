% Haskell Platform for Linux
%

**Community supported versions of the Haskell Platform:**

![](http://hackage.haskell.org/platform/icons/debian.png) [Debian]
![](http://hackage.haskell.org/platform/icons/fedora.png) [Fedora]
![](http://hackage.haskell.org/platform/icons/arch.png) [Arch Linux]
![](http://hackage.haskell.org/platform/icons/gentoo.png) [Gentoo]
![](http://hackage.haskell.org/platform/icons/nixos.png) [NixOS]

[Debian]: http://packages.debian.org/sid/haskell-platform
[Fedora]: https://admin.fedoraproject.org/pkgdb/acls/name/haskell-platform
[Arch Linux]: http://aur.archlinux.org/packages.php?ID=26279
[Gentoo]: http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc6102.haskellPlatform/jobstatus

**Information for other systems:**

![](http://hackage.haskell.org/platform/icons/ubuntu.png) Ubuntu ([Karmic], [Jaunty])
![](http://hackage.haskell.org/platform/icons/opensuse.png) [openSUSE]

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[openSUSE]: http://en.opensuse.org/Packaging/Haskell

**More info...**

* [The Haskell Platform] home...
* [Learn more] about the Haskell Platform.

[The Haskell Platform]: index.html
[Learn more]: contents.html

**Build from source**

For Unix systems (including Mac OS X), there is a source installer.

![](http://hackage.haskell.org/platform/icons/source.png)
<a href="http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2010.1.0.0.tar.gz</a>

You need GHC 6.12.1 installed before building the platform. You can get this from your distro or
you can get a [GHC 6.12.1 generic binary].

[GHC 6.12.1 generic binary]: http://haskell.org/ghc/download_ghc_6_12_1.html#distros

To install from source download and unpack the installer, then (possibly with 'sudo'):

        ./configure
        make
        make install

You may pass --prefix flags to change the default install path.

**Build from cabal**

If you already have a reasonable Haskell development environment with
GHC 6.12 and cabal-install, you can build the platform
from the Cabal package alone.

![](http://hackage.haskell.org/platform/icons/cabal.png)
<a href="http://hackage.haskell.org/platform/2010.1.0.0/cabal/haskell-platform-2010.1.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/cabal'); ">Haskell Platform Cabal Package</a>

To install, unpack the cabal tarball, and run:

        cabal install

**Older Releases**

Older releases of the Haskell Platform are available:

* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2009.2.0.2</a> source, July 2009.
* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0.1/haskell-platform-2009.2.0.1.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2009.2.0.1</a> source, June 2009.
* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0/haskell-platform-2009.2.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source/old'); ">HP 2009.2.0</a> source, May 2009.

