% Haskell Platform for Linux
%

Haskell Platform for Linux
-----

**Community supported versions of the Haskell Platform:**

* [Arch Linux]
* [Debian]
* [Fedora]
* [Gentoo]
* [NixOS]

[Arch Linux]: http://aur.archlinux.org/packages.php?ID=26279
[Gentoo]: http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform
[NixOS]: http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc6102.haskellPlatform/jobstatus
[Fedora]: http://fedoraproject.org/wiki/Haskell_SIG#Haskell_Platform_support 
[Debian]: http://packages.debian.org/sid/haskell-platform

**Experimental installers for other systems:**

* Ubuntu ([Karmic], [Jaunty])
* [OpenSUSE]

[Jaunty]: http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html
[Karmic]: http://davidsiegel.org/haskell-platform-in-karmic-koala/
[OpenSUSE]: http://en.opensuse.org/Packaging/Haskell

**Build from source**

For Unix systems (including Mac OS X), there is a source installer.

 * [haskell-platform-2010.0.1.0.tar.gz]

You need GHC 6.12.1 installed before building the platform. You can get this from your distro or
you can get a [GHC 6.12.1 generic binary].

[haskell-platform-2010.0.1.0.tar.gz]: //code.haskell.org/~dons/code/haskell-platform/snapshot/haskell-platform-2010.1.0.0-20100320-2.tar.gz
[GHC 6.12.1 generic binary]: http://haskell.org/ghc/download_ghc_6_12_1.html#distros

To install from source download and unpack the installer, then (possibly with 'sudo'):

        ./configure
        make
        make install

You may pass --prefix flags to change the default install path.
