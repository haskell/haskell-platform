% Haskell Platform for Linux
%

Haskell Platform for Linux
-----

**Community supported versions of the Haskell Platform:**

![](http://hackage.haskell.org/platform/new/icons/debian.png) [Debian]
![](http://hackage.haskell.org/platform/new/icons/fedora.png) [Fedora]
![](http://hackage.haskell.org/platform/new/icons/arch.png) [Arch Linux]
![](http://hackage.haskell.org/platform/new/icons/gentoo.png) [Gentoo]
![](http://hackage.haskell.org/platform/new/icons/nixos.png) [NixOS]

[Debian]: http://packages.debian.org/sid/haskell-platform
[Fedora]: https://admin.fedoraproject.org/pkgdb/acls/name/haskell-platform
[Arch Linux]: http://aur.archlinux.org/packages.php?ID=26279
[Gentoo]: http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc6102.haskellPlatform/jobstatus

**Information for other systems:**

![](http://hackage.haskell.org/platform/new/icons/ubuntu.png) Ubuntu ([Karmic], [Jaunty])

![](http://hackage.haskell.org/platform/new/icons/opensuse.png) [openSUSE]

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[openSUSE]: http://en.opensuse.org/Packaging/Haskell

**Build from source**

For Unix systems (including Mac OS X), there is a source installer.

![](http://hackage.haskell.org/platform/new/icons/source.png) [haskell-platform-2010.1.0.0.tar.gz]

You need GHC 6.12.1 installed before building the platform. You can get this from your distro or
you can get a [GHC 6.12.1 generic binary].

[haskell-platform-2010.1.0.0.tar.gz]: http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0.tar.gz
[GHC 6.12.1 generic binary]: http://haskell.org/ghc/download_ghc_6_12_1.html#distros

To install from source download and unpack the installer, then (possibly with 'sudo'):

        ./configure
        make
        make install

You may pass --prefix flags to change the default install path.
