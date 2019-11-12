<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Haskell Platform {{hpVersion}}</title>
    <link href="ocean.css" rel="stylesheet" type="text/css"/>
    <style type="text/css">
body {
  color: #424242;
}

h1, h2, h3, h4, h5, h6, b {
  color: #cc2900;
}

h1, h2, h3, h4, h5, h6 {
  font-family: "Helvetica Neue", Arial, Helvetica, Geneva, sans-serif;
}

#content {
  padding-top: 1em;
  position: relative;
}

p {
  margin: 0.5em 0;
}

.paths {
  margin: 0 0 0.5em 3em;
	font-family:monospace;
	*font-size:108%;
	line-height: 124%;
}

#content a {
  font-weight: bold;
  text-decoration: none;
  color: #30f
}

#content a:hover {
  background-color: #fd6;
  border: 2px solid #fd6;
}

#content.with-sidebar {
  padding-right: 15em;
}

#sidebar {
  width: 14em;
  position: absolute;
  top: 2em;
  right: 0;
  border-left: 1px dashed #cc2900;
}

#sidebar .module {
  font-size: 85%; /* 11pt */
  line-height: 1.25em;
  margin: 0 1.5em;
}

#sidebar .module + .module {
  margin-top: 1.5em;
  padding-top: 1.5em;
  border-top: 1px dashed #cc2900;
}

#sidebar ul {
  margin: 1em 0;
}

#sidebar li {
  list-style-type: none;
  padding: 0;
  margin: 0.5em 0;
  display: block;
}
    </style>
  </head>
  <body>
    <div id="package-header">
      <p class="caption">Haskell Platform  {{hpVersion}}</p>
    </div>
    <div id="content" class="with-sidebar">
      <h1>Haskell Platform {{hpVersion}} for Mac OS X</h1>
      <p>Welcome to Haskell Platform. The platform consists of the Glasgow Haskell Compiler (GHC {{ghcVersion}}) and an extensive set of standard libraries and utilities with full documentation.</p>

      <h2>Documentation</h2>
      <ul>
        <li>
          <p><a href="index.html">Libraries</a></p>
          <p>Documentation for the libraries that come with Haskell Platform &amp; GHC.</p>
        </li>
        <li>
          <p><a href="ghc-doc/users_guide/index.html">GHC</a></p>
          <p>The GHC User's Guide has all you need to know about using GHC: command line options, language extensions, GHCi, etc.</p>
        </li>
        <li>
          <p><a href="ghc-api/index.html">GHC API</a></p>
          <p>Documentation for the GHC API.</p>
        </li>
        <li>
          <p><a href="http://www.haskell.org/cabal/users-guide/">Cabal</a></p>
          <p>An infrastructure for building and distributing Haskell software.</p>
        </li>
        <li>
          <p><a href="http://docs.haskellstack.org/">Stack</a></p>
          <p>A cross-platform program for developing Haskell projects.</p>
        </li>
	<li>
          <p><a href="ghc-doc/haddock/index.html">Haddock</a></p>
          <p>A tool for automatically generating documentation from annotated Haskell source code.</p>
        </li>
      </ul>

      <h2>What is Installed</h2>
      <p>On Mac OS X, the Haskell Platform is installed in two major pieces: GHC and Haskell Platform. They are installed respectively in:</p>
      <p class="paths">/Library/Frameworks/GHC.framework
      <br/>/Library/Haskell</p>
      <p>Executables are symlinked in <tt>/usr/local/bin</tt> and should be available in any shell.</p>

      <h2>Versions &amp; Uninstallation</h2>
      <p>This and prior versions of GHC and Haskell Platform can be found and then easily removed with the uninstallation command line utility:</p>
      <p class="paths">/Library/Haskell/bin/uninstall-hs</p>
      <p>This release includes an experimental command line utility to switch between multiple installed versions of the platform:</p>
      <p class="paths">/Library/Haskell/bin/activate-hs</p>
      <p>Both utilities are safe to run with no arguments, and will give you more information when you do.</p>

      <h2>Configuring Cabal</h2>
      <p>The <tt>cabal</tt> command manages the building and installation of packages, both your own, and those it can fetch from the Hackage repository.</p>
      <p>The first time you run <tt>cabal</tt>, a Mac specific configuration is written into the <tt>~/.cabal</tt> directory.</p>
      <ul>
        <li><p>If this is the first time you have ever run <tt>cabal</tt>, it will be made your active configuration.</p></li>
        <li><p>If you have run cabal in the past, the new settings are in the file <tt>config.platform</tt>. You might want to review and incorporate some of the settings into your existing <tt>config</tt> file, or just replace your <tt>config</tt> file with it entirely.</p></li>
      </ul>
      <p>The configuration sets up <tt>cabal</tt> to install packages with the same layout as those installed with the Platform. Packages installed per user (<tt>--user</tt>, the default) are placed in a parallel tree in <tt>~/Library/Haskell</tt>.</p>
      <p><b>N.B.</b> Built executables will be symlink'd into <tt>~/Library/Haskell/bin</tt>, you probably want to add that to your <tt>$PATH</tt> by adding this line to your <tt>~/.bash_profile</tt>:
      <p class="paths">export PATH="$HOME/Library/Haskell/bin:$PATH"</p>

      <p><b>N.B.</b> Built executables by the stack tool will be symlink'd into <tt>~/.local/bin</tt>, you probably want to add that to your <tt>$PATH</tt> by adding this line to your <tt>~/.bash_profile</tt>:
      <p class="paths">export PATH="$HOME/.local/bin:$PATH"</p>

      <p>In particular, when you upgrade to a new version of stack via
      the "stack upgrade" command (and you should consider doing so if
      the installed stack version is well behind the currently
      released one), it will be installed into the above directory,
      and will only be used if that directory is placed appropriately
      in your path.</p>

      <div id="sidebar">
      <div class="module">
      <h2>Community</h2>
      <ul>
        <li><a href="http://www.haskell.org/">Haskell Home Page</a></li>
        <li>
          <a href="http://www.reddit.com/r/haskell/">Reddit</a>
        </li>
        <li>
          <a href="http://stackoverflow.com/questions/tagged?tagnames=haskell">Stack Overflow</a>
        </li>
      </ul>
      </div>
      <div class="module">
      <h2>Development Resources</h2>
      <ul>
        <li><a href="http://hackage.haskell.org/">Hackage library database</a></li>
        <li><a href="http://haskell.org/hoogle/">Hoogle</a> and
            <a href="http://holumbus.fh-wedel.de/hayoo/">Hayoo</a> API search</li>
      </ul>
      </div>
      <div class="module">
      <h2>External Documentation</h2>
      <ul>
        <li><a href="http://www.haskell.org/ghc/">GHC Home Page</a></li>
        <li><a href="http://hackage.haskell.org/trac/ghc/">GHC Developers Home</a></li>
        <li><a href="http://hackage.haskell.org/platform/">Haskell Platform</a></li>
      </ul>
      </div>
    </div>
    </div>
  </body>
</html>
