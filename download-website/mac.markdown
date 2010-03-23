% Haskell Platform for OSX
%

For Mac OS X Leopard (10.5), Snow Leopard and above (including 64 bit):

* <a id="download" href="http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0-i386.dmg" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); ">Download Haskell for Mac OS X</a>

After downloading:

* Open the .dmg file
* Follow the install instructions

**MacPorts**

The Haskell Platform is also in [MacPorts].  If you have MacPorts installed,
you can build the Haskell Platform by typing:

         sudo port install haskell-platform

[MacPorts]: http://macports.org

**Build from Source**

If you already have a GHC 6.12, but not the full platform, you can build
it from source on the Mac:

![](http://hackage.haskell.org/platform/icons/source.png)
<a href="http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2010.1.0.0.tar.gz</a>

You need GHC 6.12.1 installed before building the platform.

To install from source download and unpack the installer, then (possibly with 'sudo'):

        ./configure
        make
        make install

You may pass --prefix flags to change the default install path.
