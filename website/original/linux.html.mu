<!DOCTYPE html>
<head>
    {{> header}}
    <title>Haskell Platform for Linux</title>
</head>
<body>
<div id="header">
<h1 class="title">Haskell Platform for Linux</h1>
</div>
<table>
<tbody>
<tr class="odd">
<td align="left">&lt; <a href="index.html">Home</a></td>
</tr>
</tbody>
</table>

<p>Haskell Platform for Linux should work with any modern Linux system.

{{#current}}
<ul>
  {{#files}}{{#isLinux}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/linux'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isLinux}}{{/files}}
</ul>
{{/current}}

<blockquote>
<small>
<b>Success reported with:</b> Ubuntu 12.02, Debian 7, Fedora 20, openSUSE 13
<!-- </br><b>Reported incompatible with:</b> Fedora 20 -->
</small>
</blockquote>

<p>To install the .tar.gz archive, run:</p>
<pre><code>    tar xvf ...downloaded-tar-file...
    sudo ./install-haskell-platform.sh</code>
</pre>

<p>
The Haskell Platform requires the <code>libz</code>, <code>libgmp10</code>, the <code>freeglut3</code> libraries and development tools. To install these components run the following shell commands:
<ul class="notes">
  <li> on Debian / Ubuntu systems:
<pre><code>apt-get install -y build-essential zlib1g-dev libgmp10-dev freeglut3</code>
</pre>
  <li> on RPM-based systems:
<pre><code>yum groupinstall 'Development Tools'
yum install -y gmp-devel zlib-devel freeglut</code>
</pre>
</ul>

<p><a href="prior.html">Prior releases</a></p>

<hr />
<!--

<h2 id=community">Community Distributions</h2>
<p>These distributions offer the Haskell Platform in their package repositories.</p>
<table>
<tbody>
<tr class="odd">
<td align="left"><img src="icons/ubuntu.png" /> <a href="http://packages.ubuntu.com/haskell-platform">Ubuntu</a></td>
<td align="left"><img src="icons/debian.png" /> <a href="http://packages.debian.org/haskell-platform">Debian</a></td>
<td align="left"><img src="icons/fedora.png" /> <a href="https://apps.fedoraproject.org/packages/haskell-platform">Fedora</a></td>
<td align="left"><img src="icons/gentoo.png" /> <a href="http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform">Gentoo</a></td>
<td align="left"><img src="icons/nixos.png" /> <a href="http://hydra.nixos.org/job/nixpkgs/trunk/haskellPlatform">NixOS</a></td>
</tr>
<tr class="even">
<td align="left"><img src="icons/openbsd.png" /> <a href="http://openports.se/meta/haskell-platform">OpenBSD</a></td>
<td align="left"><img src="icons/freebsd.png" /> <a href="http://www.freshports.org/devel/hs-haskell-platform/">FreeBSD</a></td>
<td align="left"><img src="icons/mint.png" /> <a href="http://community.linuxmint.com/software/view/haskell-platform">Mint</a></td>
<td align="left"><img src="icons/arch.png" /> <a href="https://wiki.archlinux.org/index.php/Haskell#Haskell_platform">Arch Linux</a></td>
<td align="left"><img src="icons/opensuse.png" /> <a href="https://build.opensuse.org/project/show?project=devel:languages:haskell">openSUSE</a></td>
</tr>
</tbody>
</table>
<p>See also: <em><a href="http://justhub.org/">justhub</a></em>, for RHEL, CentOS, Scientific Linux, and Fedora support</p>

<hr />
-->

{{> footer}}
</body>
</html>
