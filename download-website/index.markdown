% Haskell Platform Download <img src="http://code.haskell.org/haskell-platform/download-website/images/beta-icon.png" alt="&Beta;">
%

<img src="http://haskell.org/sitewiki/images/a/a8/Haskell-logo-60.png" alt="Logo">

This is the beta release of the [Haskell Platform], version 2009.2.0.

The Haskell Platform is a blessed library and tool suite for Haskell.
Taking the best software from [Hackage], it provides a comprehensive,
stable and mature base for Haskell projects to work from. Every system
supporting "Haskell" will need to comply to the Haskell Platform
specification. We think every OS distribution should ship with the
Haskell Platform!

Please note that this is a beta release. We do not expect everything to
work perfectly and we would appreciate feedback. Issues related to the
packaging and installers can be filed in the [platform trac].

[Haskell Platform]: http://haskell.org/haskellwiki/Haskell_Platform
[Hackage]: http://hackage.haskell.org
[platform trac]: http://trac.haskell.org/haskell-platform/

There will be a couple more minor releases in the 2009.2.x series. These
will incorperate your feedback on the installers. They will also include
minor bug-fix updates in some packages.


Windows
-------

haskell-platform-2009.2.0.exe

MacOS X
-------

The installer for MacOS X is not yet available. 

Linux
-----

A few distributions already support the Haskell Platform. We expect more to follow soon.

 * Arch
 * Gentoo
 * Nix / NixOS

If your distribution is not yet listed then you will need to use the source tarball.

Source
------

For unix systems, there is a generic source installer. You only need GHC
installed to get started:

haskell-platform-2009.2.0.tar.gz

This is a traditional "./configure; make; make install" style .

The source tarball requires that you already have ghc-6.10.x installed.

More info
---------

* [Haskell Platform]
* [platform trac]
