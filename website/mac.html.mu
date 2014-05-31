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
<ul>
<li><p><strong><a href="download/2013.2.0.0/Haskell%20Platform%202013.2.0.0%2032bit.pkg" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); ">Haskell Platform 2013.2.0.0 for Mac OS X, 32 bit</a></strong> (10.6 or later) <br /><small>SHA-1: <code>15dd8762c9800308cb7cfdd16ea1a8e74988e06a</code></small></p></li>
<li><p><strong><a href="download/2013.2.0.0/Haskell%20Platform%202013.2.0.0%2064bit.pkg" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); "> Haskell Platform 2013.2.0.0 for Mac OS X, 64 bit</a></strong> (10.6 or later) <br /><small>SHA-1: <code>89e6fb747816af69acabc5c04cee103257855614</code></small></p></li>
</ul>
<!--
* <small>*Pick the 32-bit version, unless you have a specific reason to use the 64-bit version.*<br />
The 32-bit one is slightly faster for most programs.<br />
If you use **MacPorts**, **brew**, or other 3rd party libraries, see below.</small>
-->

<p>After downloading:</p>
<ul>
<li>Double click the package icon to start the installer.</li>
<li>Follow the instructions.</li>
</ul>
<hr />
<p><strong>Xcode 5 &amp; OS X 10.9 (Mavericks)</strong></p>
<p>Xcode 5 no longer includes <em>gcc</em>. GHC 7, whether installed directly or via Haskell Platform, can be made to work with this wrapper script:</p>
<ol style="list-style-type: decimal">
<li>Copy this script to <tt>/usr/bin</tt>, and make sure it is exectuable.</li>
<li>Run it <tt>sudo</tt>. (Running without <tt>sudo</tt> will tell you what it would do if you did.)</li>
</ol>
<ul>
<li>Download here: <a href="ghc-clang-wrapper">ghc-clang-wrapper</a> bash script</li>
</ul>
<p>Xcode 5 supplies <em>clang</em> as the C compier, and with respect to pre-processing there are some &quot;differences of interpretation&quot; between it and <em>gcc</em>. These differences can affect some Haskell code that uses the <tt>CPP</TT> extension. An alternate approach for users with Xcode 5 is to install a <em>gcc</em> and direct GHC at that. See <a href="https://gist.github.com/cartazio/7131371">this page</a> for instructions.</p>
<hr />
<p><strong>Notes:</strong></p>
<p><em>Only one architecture (32-bit or 64-bit) can be installed at a time.</em></p>
<p><em>The command line development tools are required prior to installation.</em> <br />If you have <tt>/usr/bin/gcc</tt> available in a shell, you should be good to go. If not:</p>
<ul>
<li><p>On OS 10.6, 32-bit, use Xcode 3.2 or later: Choose the &quot;Customize…&quot; button during installation and choose <strong>UNIX Development</strong>. 64-bit version requires Xcode 4.1 or later.</p></li>
<li><p>On OS 10.7 or later, choose one of the following:</p>
<ul>
<li><p>Download and install <strong>Command Line Tools for Xcode</strong>. Despite the name, you don't need Xcode installed!</p></li>
<li><p>After installing Xcode (4.3 or later), choose <strong>Preferences</strong>, then pick the <strong>Downloads</strong> panel. There you can download and install the <strong>Command Line Tools</strong> as an optional component. This installs the same package as listed in the above option.</p></li>
<li><p>If you have Xcode prior to 4.3, depending on how you installed it, you may already have the command line tools. If not, consider the first option above.</p></li>
</ul></li>
<li><p><em>The command line tools and/or Xcode can be downloaded for free from <a href="http://developer.apple.com">Apple's developer website</a>, you do need to register as a developer (also free).</em></p></li>
</ul>
<p><em>MacPorts, brew, and other native libraries:</em></p>
<ul>
<li><p>Install the version (32-bit or 64-bit) that matches the architecture of the libraries you intend to work with.</p></li>
<li><p>By default, MacPorts, brew and most libraries only build 64-bit versions on 64-bit capable systems. Hence you need to pick the 64-bit version of the platform to use it with these libraries.</p></li>
<li><p>You might be able to install libraries &quot;universal&quot;, which will make them work with either version of the platform, but not all libraries build this this way. Try the commands <tt>port install libfoo +universal</tt> or <tt>brew install libfoo --universal</tt></p></li>
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
