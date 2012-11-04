% Haskell Platform for Mac OS X
%

-------------------------------
< [Home]
-------------------------------

[Home]: index.html

<img style="float:right;" src="images/OS_X-Logo.png" />
Download the Mac OS X installer here:

* **<a href="http://lambda.haskell.org/platform/download/2012.2.0.0/Haskell%20Platform%202012.2.0.0%2032bit.pkg" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); ">Haskell Platform 2012.2.0.0 for Mac OS X, 32 bit</a>** (10.6 or later)

* **<a href="http://lambda.haskell.org/platform/download/2012.2.0.0/Haskell%20Platform%202012.2.0.0%2064bit.pkg" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); "> Haskell Platform 2012.2.0.0 for Mac OS X, 64 bit</a>** (10.6 or later)

* <small>*Pick the 32-bit vesion, unless you have a specific reason to use the 64-bit version.*<br />
The 32-bit one is slightly faster for most programs.<br />
If you use **MacPorts**, **brew**, or other 3rd party libraries, see below.</small>

After downloading:

* Double click the package icon to start the installer.
* Follow the instructions.

----

**Notes:**

*Only one architecture (32-bit or 64-bit) can be installed at a time.*

*The command line development tools are required prior to installation.*
<br />If you have <tt>/usr/bin/gcc</tt> available in a shell, you should be good to go. If not:

  * On OS 10.6, use Xcode 3.2 or later: Choose the "Customize…" button during installation and choose **UNIX Development**

  * On OS 10.7, choose one of the following:

    * Download and install **Command Line Tools for Xcode**.
      Despite the name, you don't need Xcode installed!

    * After installing Xcode 4.3, choose **Preferences**, then
      pick the **Downloads** panel. There you can download and
      install the **Command Line Tools** as an optional component.
      This installs the same package as listed in the above option.

    * If you have Xcode prior to 4.3, depending on how you installed it,
      you may already have the command line tools. If not, consider the
      first 10.7 option above.

  * *The command line tools and/or Xcode can be downloaded for free
    from [Apple's developer website](http://developer.apple.com), you
    do need to register as a developer (also free).*

*MacPorts, brew, and other native libraries:*

  * Install the version (32-bit or 64-bit) that matches the architecture of the
    libraries you intend to work with.

  * By default, MacPorts, brew and most libraries only build 64-bit versions on
     64-bit capable systems. Hence you need to pick the 64-bit version of the
     platform to use it with these libraries.

  * You might be able to install libraries "universal", which will make them
    work with either version of the platform, but not all libraries build this
    this way. Try the commands <tt>port install libfoo +universal</tt> or
    <tt>brew install libfoo --universal</tt>

*For Mac OS X 10.5 please use the 2010.2.0.0 installer:*

* <a href="http://lambda.haskell.org/platform/download/2010.2.0.0/haskell-platform-2010.2.0.0.i386.dmg" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">Haskell Platform 2010.2.0.0 for Mac OS X, 10.5</a>

--------

[Prior releases](prior.html)

