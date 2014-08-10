<!DOCTYPE html>
<head>
    {{> header}}
    <title>Haskell Platform for Mac OS X</title>
</head>
<body>
<div id="header">
<h1 class="title">Haskell Platform for Mac OS X</h1>
</div>
<table>
<tbody>
<tr class="odd">
<td align="left">&lt; <a href="index.html">Home</a></td>
</tr>
</tbody>
</table>
<p><img style="float:right;" src="images/OS_X-Logo.png" /> Download the Mac OS X installer here:</p>

{{#current}}
<ul>
  {{#files}}{{#isOSX}}
  <li><p><strong><a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); ">Haskell Platform {{version}} for {{osNameAndArch}}</a></strong> (10.6 or later)
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isOSX}}{{/files}}
</ul>
{{/current}}

<p>After downloading:</p>
<ul>
<li>Double click the package icon to start the installer.</li>
<li>Follow the instructions.</li>
</ul>

<hr />
<h2>Release Notes:</h2>

<ul>
  <li>Works on 10.6, 10.7, 10.8, 10.9, and even 10.10 beta!</li>
  <li>Works with both gcc and clang based systems.</li>
  <li>Distributed with a build of GHC 7.8.3 that differs from the released
    bindist in two ways: a) it was built split-objs for smaller resulting
    executables, b) it includes Cabal-1.18.1.4 which fixes a particularly nasty
    problem with haddock, -XCPP, and clang based systems. This ghc-7.8.3 bindist
    is already incorporated into the installer above, but for reference is
    available here: <a href="download/2014.2.0.0/ghc-7.8.3-x86_64-apple-darwin-r3.tar.bz2">ghc-7.8.3-x86_64-apple-darwin-r3.tar.bz2</a></li>

  <li>Includes a new experimental <code>activate-hs</code> command that can
    switch between multiple installed versions of the platform</li>

  <li>The cabal command is wrapped to provide a smoother file layout on the Mac.
    The wrapping only updates the <code>~/.cabal/config</code> file the first
    time you run it. Please pay attention to its output. If you have a custom
    config file, you'll want to update it, as Cabal's defaults have changed.</li>

</ul>


<h2>General Notes:</h2>

<p><em>The command line development tools are required prior to installation.</em> <br />If you have <tt>/usr/bin/ld</tt> available in a shell, you should be good to go. If not:</p>
<ul>
<li><p>On OS 10.7 or later, choose one of the following:</p>
<ul>
<li><p>Download and install <strong>Command Line Tools for Xcode</strong>. Despite the name, you don't need Xcode installed!</p></li>
<li><p>After installing Xcode (4.3 or later), choose <strong>Preferences</strong>, then pick the <strong>Downloads</strong> panel. There you can download and install the <strong>Command Line Tools</strong> as an optional component. This installs the same package as listed in the above option.</p></li>
<li><p>If you have Xcode prior to 4.3, depending on how you installed it, you may already have the command line tools. If not, consider the first option above.</p></li>
</ul></li>
<li><p>On OS 10.6, 32-bit, use Xcode 3.2 or later: Choose the &quot;Customize…&quot; button during installation and choose <strong>UNIX Development</strong>. 64-bit version requires Xcode 4.1 or later.</p></li>
<li><p><em>The command line tools and/or Xcode can be downloaded for free from <a href="http://developer.apple.com">Apple's developer website</a>, you do need to register as a developer (also free).</em></p></li>
</ul>

<p><em>For Mac OS X 10.5 please use the 2010.2.0.0 installer:</em></p>
<ul>
<li><a href="download/2010.2.0.0/haskell-platform-2010.2.0.0.i386.dmg" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">Haskell Platform 2010.2.0.0 for Mac OS X, 10.5</a></li>
</ul>
<hr />
<p><a href="prior.html">Prior releases</a></p>
{{> footer}}
</body>
</html>
