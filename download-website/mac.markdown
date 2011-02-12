% Haskell Platform for OSX
%

The Haskell Platform installer for Mac OS X Leopard (10.5), Snow Leopard
and above (including 64 bit), Intel only, via BitTorrent.  Please consider using the torrent for a faster download:

* <a id="download" href="http://hackage.haskell.org/platform/2010.2.0.0/torrents/haskell-platform-2010.2.0.0.i386.dmg.torrent" onClick="javascript: pageTracker._trackPageview('/downloads/torrent/mac'); ">Torrent for Mac OS X (intel)</a>

For Mac OS X Leopard (10.5), Snow Leopard and above (including 64 bit),
Intel only:

* <a id="download" href="http://lambda.haskell.org/hp-tmp/2010.2.0.0/haskell-platform-2010.2.0.0.i386.dmg" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); ">Download Haskell for Mac OS X (Intel)</a>

After downloading:

* Open the .dmg file
* Follow the install instructions

**More info...**

* [The Haskell Platform] home...
* [Learn more] about the Haskell Platform.

[The Haskell Platform]: index.html
[Learn more]: contents.html

**MacPorts**

The Haskell Platform is also in [MacPorts].  If you have MacPorts installed,
you can build the Haskell Platform by typing:

         sudo port install haskell-platform

[MacPorts]: http://macports.org

Note the version of the Haskell Platform in MacPorts may be lagging the
current release.

**Build from Source**

If you already have a GHC 6.12.3, but not the full platform, you can build
it from source on the Mac:

![](http://hackage.haskell.org/platform/icons/source.png)
<a href="http://hackage.haskell.org/platform/2010.2.0.0/haskell-platform-2010.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2010.2.0.0.tar.gz</a>

You need GHC 6.12.3 installed before building the platform.

To install from source download and unpack the installer, then (possibly with 'sudo'):

        ./configure
        make
        make install

You may pass --prefix flags to change the default install path.

**Older Releases**

Older releases of the Haskell Platform are available:

* <a id="download" href="http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.1-i386.dmg" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">HP 2010.1.0.0</a>, for Mac OS X Leopard (intel), March 2010.
* <a id="download" href="http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2-i386.dmg" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">HP 2009.2.0.2</a>, for Mac OS X Leopard (intel), July 2009.
