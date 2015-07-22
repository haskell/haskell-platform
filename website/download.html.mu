<!DOCTYPE html> <!-- -*- mode: web-mode; engine: ctemplate -*- -->
<html>
    <head>
        {{> head }}
        <link rel="stylesheet" type="text/css" href="stylesheets/download.css">

        <meta name="description" content="Haskell Platform is a Haskell distribution with batteries included">
        <script src="js/download.js"></script>
        <title>Download Haskell Platform</title>
    </head>
    <body class="page-home">
        <div class="wrap">
            <div class="template">
                {{> navbar }}

                <div class="header">
                    <div class="container">
                        <div class="row">
                            <div class="span6 col-md-6">
                                <div class="branding">
                                    <span style="background-image: url(img/logo.png)" class="name">Haskell Platform</span>
                                    <span class="summary">
                                        Haskell with batteries included
                                    </span>
                                </div>
                            </div>
                            <div class="span6 col-md-6">
                                <div class="branding">
                                    <span class="tag">
                                        <h3>A multi-OS distribution</h3>

                                        <p>designed to get you up and running
                                            quickly, making it easy to focus on using
                                            Haskell. You get:</p>
                                        <ul>
                                            <li>the <a href="http://www.haskell.org/ghc/">Glasgow Haskell Compiler</a></li>
                                            <li>the <a href="http://www.haskell.org/cabal/">Cabal build system</a></li>
                                            <li>support for profiling and code coverage analysis</li>
                                            <li>35 core & widely-used <a href="contents.html">packages</a></li>
                                        </ul>
                                    </span>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="download-options">
                    <div class="container">
                        <div class="row">
                            <div class="span4 col-md-4 vrule get-hp">
                                <a href="#get-started">
                                    <div style="width: 50%; font-size: 100px; float:right; ">
                                        <i class="fa fa-arrow-down"></i>
                                    </div>
                                    <h1 style="width: 50%;">Download Haskell Platform</h1>
                                </a>
                            </div>
                            <div class="span4 col-md-4 other-ways">
                                <h1>Libraries</h1>
                                <p>Haskell has a vibrant community and a rich
                                collection of packages ripe for the picking on
                                <a href="http://hackage.haskell.org/">Hackage</a>,
                                the Haskell package database.</p>
                            </div>
                            <div class="span4 col-md-4 other-ways vrule">
                                <h1>Other ways to install</h1>
                                <p>While the Haskell Platform is the recommended
                                way to get a Haskell environment, there are also
                                other options</p>
                                <ul>
                                    <li><a href="https://www.haskell.org/downloads/windows">Windows</a></li>
                                    <li><a href="https://www.haskell.org/downloads/osx">OS X</a></li>
                                    <li><a href="https://www.haskell.org/downloads/linux">Linux</a></li>
                                </ul>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="container">
                    <h2 id="get-started">Let's get started</h2>
                </div>

                <div class="container found-user-platform" >
                    You appear to be using <strong>unknown</strong>.
                    See <a href="#other-platforms">below</a> for other
                    operating systems.
                </div>

                <div class="container unknown-user-platform" >
                    Find your operating system of choice below and follow the
                    instructions to install the Haskell Platform on your system.
                </div>

                <div class="container platform-toc">
                    <ul>
                        <li><a href="#osx"><img src="img/os-osx.svg" alt="Mac OS X logo"> Mac OS X</a></li>
                        <li><a href="#windows"><img src="img/os-windows.svg" alt="Windows logo"> Windows</a></li>
                        <li><a href="#linux"><img src="img/os-linux.svg" alt="Linux logo"> Linux</a></li>
                    </ul>
                </div>

                <div class="container" id="platforms">
                    <h2 id="other-platforms" class="other-platforms">Other Operating Systems</h2>

                    <section class="downloads-platform container" data-os="osx" id="osx">
                        <div class="platform-name"><img src="img/os-osx.svg" alt="Mac OS X logo"><h2>Mac OS X</h2></div>
                        <a class="expander" href="#osx"><div>
                            <img src="img/expand-piece.svg" class="expand-1">
                            <img src="img/expand-piece.svg" class="expand-2">
                            <img src="img/expand-piece.svg" class="expand-3">
                        </div></a>

                        <div class="sidebar flavors">
                            <strong>Choose your package manager</strong>
                            <ul>
                                <li class="active"><a href="#osx-none"><span class="logo"><i class="fa fa-cogs"></i></span>None</a></li>
                                <li><a href="#osx-macports"><img alt="MacPorts logo" class="logo" src="img/distro-macports.png">MacPorts</a></li>
                            </ul>
                        </div>

                        <div class="content">
                            <div id="osx-none" class="flavor active">
                                <p>
                                    The latest version of the Haskell Platform for Mac OS X is
                                    <strong>{{hpVersion}}</strong>. Note that the
                                    Haskell Platform is only compatible with
                                    <strong>OS X 10.6 and later</strong>.
                                </p>
                                <p>
                                    These packages are for Mac OS X systems not
                                    using a package manager. If you would rather
                                    install with MacPorts then select the
                                    appropriate option to the right.
                                </p>
                                <p> To get started perform these steps,</p>

                                <ol class="install-steps">
                                    <li>
                                        <div class="step-number">1</div>
                                        <div class="step-body"
                                            <p>Download the installer disk image,</p>
                                            {{#current}} {{#files}} {{#isOSX}}
                                            <div class="download-btn">
                                                <a href="{{url}}" class="btn btn-haskell" role="button">
                                                    <i class="fa fa-download"></i> Download ({{archBits}} bit)
                                                </a>
                                            </div>
                                            {{/isOSX}} {{/files}} {{/current}}

                                            <div class="download-hash">
                                                You can verify the authenticity of this file by
                                                checking its <strong>SHA-1</strong> hash,
                                                <ul class="hashes">
                                                    {{#current}} {{#files}} {{#isOSX}}
                                                    <li>{{archBits}} bit: <input readonly class="file-hash" type="text" value="{{mHash}}"></li>
                                                    {{/isOSX}} {{/files}} {{/current}}
                                                </ul>
                                            </div>
                                        </div>
                                    </li>
                                    <li>
                                        <div class="step-number">2</div>
                                        <div class="step-body">Run the installer.</div>
                                    </li>
                                    <li>
                                        <div class="step-number">3</div>
                                        <div class="step-body">Follow the instructions.</div>
                                    </li>
                                </ol>
                            </div> <!-- #osx-none -->

                            <div id="osx-macports" class="flavor">
                                <h3>MacPorts</h3>
                                <p>To install Haskell Platform with
                                    <a href="https://trac.macports.org/browser/trunk/dports/devel/haskell-platform/Portfile">MacPorts</a>,
                                    simply run,
                                </p>
                                <pre>$ sudo port install haskell-platform</pre>
                            </div> <!-- #osx-macports -->

                        </div>
                        <div class="bottom-rule"></div>
                    </section>

                    <section class="downloads-platform container" data-os="windows" id="windows">
                        <div class="platform-name"><img src="img/os-windows.svg" alt="Windows logo"> <h2>Windows</h2></div>
                        <a class="expander" href="#windows"><div>
                            <img src="img/expand-piece.svg" class="expand-1">
                            <img src="img/expand-piece.svg" class="expand-2">
                            <img src="img/expand-piece.svg" class="expand-3">
                        </div></a>
                        <div class="content">
                            <p>
                                The latest version of the Haskell Platform for Windows is
                                <strong>{{hpVersion}}</strong>.
                            </p>
                            <p> To get started perform these steps,</p>

                            <ol class="install-steps">
                                <li>
                                    <div class="step-number">1</div>
                                    <div class="step-body">
                                        <p>Download the installer,</p>
                                        {{#current}} {{#files}} {{#isWindows}}
                                        <div class="download-btn">
                                            <a href="{{url}}" class="btn btn-haskell" role="button">
                                                <i class="fa fa-download"></i> Download ({{archBits}} bit)
                                            </a>
                                        </div>
                                        {{/isWindows}} {{/files}} {{/current}}

                                        <div>
                                            You can verify the authenticity of this file by
                                            checking its <strong>SHA-1</strong> hash,
                                            <ul class="hashes">
                                                {{#current}} {{#files}} {{#isWindows}}
                                                <li>{{archBits}} bit: <input readonly class="file-hash" type="text" value="{{mHash}}"></li>
                                                {{/isWindows}} {{/files}} {{/current}}
                                            </ul>
                                        </div>
                                    </div>
                                </li>
                                <li>
                                    <div class="step-number">2</div>
                                    <div class="step-body">Run the installer and follow the instructions.</div>
                                </li>
                                <li>
                                    <div class="step-number">3</div>
                                    <div class="step-body">Start WinGHCi from the Start menu and have fun!</div>
                                </li>
                            </ol>
                        </div>
                        <div class="bottom-rule"></div>
                    </section>

                    <section class="downloads-platform container" data-os="linux" id="linux">
                        <div class="platform-name"><img src="img/os-linux.svg" alt="Linux logo"> <h2>Linux</h2></div>
                        <a class="expander" href="#linux"><div>
                            <img src="img/expand-piece.svg" class="expand-1">
                            <img src="img/expand-piece.svg" class="expand-2">
                            <img src="img/expand-piece.svg" class="expand-3">
                        </div></a>

                        <div class="sidebar flavors">
                            <strong>Choose your distribution</strong>
                            <ul>
                                <li><a href="#linux-generic"><span class="logo"><i class="fa fa-cogs"></i></span>Generic</a></li>
                                <li><a href="#linux-ubuntu"><img alt="Ubuntu logo" class="logo" src="img/distro-ubuntu.svg">Ubuntu</a></li>
                                <li><a href="#linux-debian"><img alt="Debian logo" class="logo" src="img/distro-debian.svg">Debian</a></li>
                                <li><a href="#linux-redhat"><img alt="Redhat logo" class="logo" src="img/distro-redhat.svg">Redhat</a></li>
                                <li><a href="#linux-fedora"><img alt="Fedora logo" class="logo" src="img/distro-fedora.svg">Fedora</a></li>
                                <li><a href="#linux-mint"><img alt="Linux Mint logo" class="logo" src="img/distro-mint.svg">Mint</a></li>
                                <li><a href="#linux-gentoo"><img alt="Gentoo Linux logo" class="logo" src="img/distro-gentoo.svg">Gentoo</a></li>
                                <li><a href="#linux-source"><span class="logo"><i class="fa fa-code"></i></span>From Source</a></li>
                            </ul>

                            <p class="select-generic">
                                If you can't find your distribution then select
                                <strong>Generic</strong>.
                            </p>
                        </div>

                        <div class="content">
                            <div id="linux-prompt" class="flavor">
                                <h3>Select your distribution</h3>
                                <p>
                                    Please select your Linux distribution in the
                                    list on the right.
                                </p>
                                <div class="point-to-flavors-list"><i class="fa fa-arrow-right"></i></div>
                            </div>

                            <div id="linux-generic" class="flavor">
                                <h3>Generic Linux</h3>
                                <p>
                                    This is a <strong>generic</strong>
                                    distribution of the Haskell Platform. While
                                    it should work on most modern Linux
                                    distributions, you should use one of the
                                    distribution-specific options listed on the
                                    right if possible.
                                </p>
                                <p>
                                    The latest version of the Haskell Platform for Linux is
                                    <strong>{{hpVersion}}</strong>.</p>
                                <p> To get started perform these steps,</p>

                                <ol class="install-steps">
                                    <li>
                                        <div class="step-number">1</div>
                                        <div class="step-body">
                                            <p>Download the installation tarball,</p>
                                            {{#current}} {{#files}} {{#isLinux}}
                                            <div class="download-btn">
                                                <a href="{{url}}" class="btn btn-haskell" role="button">
                                                    <i class="fa fa-download"></i> Download ({{archBits}} bit)
                                                </a>
                                            </div>
                                            {{/isLinux}} {{/files}} {{/current}}

                                            <div>
                                                You can verify the authenticity of this file by
                                                checking its <strong>SHA-1</strong> hash,
                                                <ul class="hashes">
                                                    {{#current}} {{#files}} {{#isLinux}}
                                                    <li>{{archBits}} bit: <input readonly class="file-hash" type="text" value="{{mHash}}"></li>
                                                    {{/isLinux}} {{/files}} {{/current}}
                                                </ul>
                                            </div>
                                        </div>
                                    </li>
                                    <li>
                                        <div class="step-number">2</div>
                                        <div class="step-body">
                                            Extract the tarball using
                                            <pre>$ sudo tar -zx -C /usr \
        -f haskell-platform-2015.01.2.tar.gz</pre>
                                        </div>
                                    </li>
                                    <!--
                                    <li>
                                        <div class="step-number">3</div>
                                        <div class="step-body">
                                            <p>Run the installation script with</p>
                                            <pre>$ ./install-haskell-platform-2015.01.2.sh</pre>

                                            <p>Or instead, if you don't have root access, you can install locally,</p>
                                            <pre>$ ./install-haskell-platform-2015.01.2.sh --user</pre>
                                        </div>
                                    </li>
                                    -->
                                </ol>
                            </div> <!-- #linux-generic -->

                            <div id="linux-ubuntu" class="flavor">
                                <h3>Ubuntu</h3>
                                <p>Good news! Haskell Platform is already
                                    available in your distribution's package
                                    <a href="http://packages.ubuntu.com/search?keywords=haskell-platform">repository</a>.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo apt-get install haskell-platform</pre>
                            </div> <!-- #linux-ubuntu -->

                            <div id="linux-debian" class="flavor">
                                <h3>Debian</h3>
                                <p>Good news! Haskell Platform is already
                                    available in your distribution's package
                                    <a href="https://packages.debian.org/search?keywords=haskell-platform">repository</a>.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo apt-get install haskell-platform</pre>
                            </div> <!-- #linux-debian -->

                            <div id="linux-mint" class="flavor">
                                <h3>Linux Mint</h3>
                                <p>Good news! Haskell Platform is already
                                    available in your distribution's package
                                    <a href="http://community.linuxmint.com/software/view/haskell-platform">repository</a>.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo apt-get install haskell-platform</pre>
                            </div> <!-- #linux-mint -->

                            <div id="linux-redhat" class="flavor">
                                <h3>Redhat</h3>
                                <p>Good news! Haskell Platform is already available in
                                your distribution's package repository.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo yum install haskell-platform</pre>
                            </div> <!-- #linux-redhat -->

                            <div id="linux-fedora" class="flavor">
                                <h3>Fedora</h3>
                                <p>Good news! Haskell Platform is already available in
                                    your distribution's package
                                    <a href="https://admin.fedoraproject.org/pkgdb/package/haskell-platform/">repository</a>.</p>
                                <p>Simply run,</p>
                                <pre>$ sudo yum install haskell-platform</pre>
                            </div> <!-- #linux-fedora -->

                            <div id="linux-gentoo" class="flavor">
                                <h3>Gentoo</h3>
                                <p>Good news! Haskell Platform is already
                                    available in your distribution's package repository.</p>
                                <p>
                                    While there is a <code>haskell-platform</code> ebuild
                                    included in the main Portage tree,
                                    it is recommended that one uses the more
                                    up-to-date <a href="https://github.com/gentoo-haskell/gentoo-haskell/tree/master/dev-haskell/haskell-platform"><code>gentoo-haskell</code></a>
                                    overlay. This can be done using <code>layman</code>,</p>
                                <pre>
$ sudo layman -a haskell
$ sudo emerge haskell-platform</pre>
                                <p>More details can be found in the
                                    <a href="https://wiki.haskell.org/Gentoo/HaskellPlatform">Wiki</a>.</p>
                            </div> <!-- #linux-gentoo -->

                            <div id="linux-source" class="flavor">
                                <h3>Build from source</h3>
                                <p>
                                    If we don't have a binary package suitable for your distribution
                                    you can build the Haskell Platform from source.
                                </p>
                                <ol class="install-steps">
                                    <li>
                                        <div class="step-number">1</div>
                                        <div class="step-body">
                                            <p>Download and extract the source tarball,</p>
                                            {{#current}} {{#files}} {{#isSource}}
                                            <div class="download-btn">
                                                <a href="{{url}}" class="btn btn-haskell" role="button">
                                                    <i class="fa fa-download"></i> Download
                                                </a>
                                            </div>
                                            {{/isSource}} {{/files}} {{/current}}

                                            <div>
                                                You can verify the authenticity of this file by
                                                checking its <strong>SHA-1</strong> hash,
                                                <ul class="hashes">
                                                    {{#current}} {{#files}} {{#isSource}}
                                                    <li><input readonly class="file-hash" type="text" value="{{mHash}}"></li>
                                                    {{/isSource}} {{/files}} {{/current}}
                                                </ul>
                                            </div>
                                        </div>
                                    </li>
                                    <li>
                                        <div class="step-number">2</div>
                                        <div class="step-body">
                                            <p>See the <code>README</code> file for build instructions.</p>
                                        </div>
                                    </li>
                                </ol>
                            </div> <!-- #linux-source -->

                        </div> <!-- linux .content -->
                        <div class="bottom-rule"></div>
                    </section>
                </div>
            </div>
        </div>

        <!-- a bit of whitespace before the footer -->
        <div style="height: 100px;"></div>

        {{> footer }}
    </body>
</html>
