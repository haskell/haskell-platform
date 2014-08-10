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

<p>There are three ways to get Haskell Platform for Linux, BSD, and other Posix
style systems:</p>
<ul>
    <li><a href="#community">Community Distribution:</a> The platform packaged up
        for the package management systems of various OS distributions. This is
        the easiest way to install the platform, but the version available in your
        OSes repositories may lag behind the current platform release.</li>
    <li><a href="#binary">Binary Distribution:</a> A build for Deb 7 style systems.
        Should work for up-to-date systems, including Ubuntu 12 and 14.</li>
    <li><a href="#source">Source Distribution:</a> Build the platform on your
        system. Requires a pre-existing, working haskell system.</li>
</ul>
<hr />

<h2 id=community">Community-supported versions of the Haskell Platform on Linux and Unix</h2>
<p>These distributions offer the Haskell Platform in their package repositories. You can easily install the Haskell Platform through your distribution's native package manager.</p>
<table>
<tbody>
<tr class="odd">
<td align="left"><img src="icons/ubuntu.png" /> <a href="http://packages.ubuntu.com/haskell-platform">Ubuntu</a></td>
<td align="left"><img src="icons/debian.png" /> <a href="http://packages.debian.org/haskell-platform">Debian</a></td>
<td align="left"><img src="icons/fedora.png" /> <a href="https://apps.fedoraproject.org/packages/haskell-platform">Fedora</a></td>
</tr>
<tr class="even">
<td align="left"><img src="icons/gentoo.png" /> <a href="http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform">Gentoo</a></td>
<td align="left"><img src="icons/nixos.png" /> <a href="http://hydra.nixos.org/job/nixpkgs/trunk/haskellPlatform">NixOS</a></td>
<td align="left"><img src="icons/openbsd.png" /> <a href="http://openports.se/meta/haskell-platform">OpenBSD</a></td>
</tr>
<tr class="odd">
<td align="left"><img src="icons/freebsd.png" /> <a href="http://www.freshports.org/devel/hs-haskell-platform/">FreeBSD</a></td>
<td align="left"><img src="icons/mint.png" /> <a href="http://community.linuxmint.com/software/view/haskell-platform">Mint</a></td>
</tr>
</tbody>
</table>
<p><strong>Information for other systems</strong></p>
<table>
<tbody>
<tr class="odd">
<td align="left"><img src="icons/arch.png" /> <a href="https://wiki.archlinux.org/index.php/Haskell#Haskell_platform">Arch Linux</a></td>
<td align="left"><img src="icons/opensuse.png" /> <a href="https://build.opensuse.org/project/show?project=devel:languages:haskell">openSUSE</a></td>
<td align="left"><img src="icons/mandriva.png" /> <a href="http://wiki.mandriva.com/en/Development/Tasks/Packaging/Policies/Haskell">Mandriva</a></td>
</tr>
</tbody>
</table>
<p>See also: <em><a href="http://justhub.org/">justhub</a></em>, for RHEL, CentOS, Scientific Linux, and Fedora support</p>

<hr />

<h2 id="binary">Generic Linux Binaries</h2>
<p>For generic, Deb 7, style systems, you can use this binary distribution:</p>

{{#current}}
<ul>
  {{#files}}{{#isLinux}}
  <li><p><strong><a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/linux'); ">Haskell Platform {{version}} for {{osNameAndArch}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isLinux}}{{/files}}
</ul>
{{/current}}

<blockquote>
<small>
<b>Success reported with:</b> Mint 17, Ubuntu 12, Ubunutu 14
</br><b>Reported incompatible with:</b> Fedora 20
</small>
</blockquote>

<p>To install this, run:</p>
<pre><code>    cd /
    sudo tar xvf ...downloaded-tarfile...
    sudo /usr/local/haskell/ghc-{{ghcVersion}}-x86-64/bin/activate-hs</code></pre>

<p>Notes:</p>
<ul>
    <li>Built on Ubuntu 12.04LTS</li>
    <li>Tested on Ubuntu 14</li>
    <li>Your system may need additional library packages installed to work</li>
    <li>Your system needs libgmp.so.10, and may require a symlink from libgmp.so to libgmp.so.10</li>
</ul>

<hr />

<h2 id="source">Build from source</h2>
<p>Download the source tarball for Unix-like systems:</p>

{{#current}}
<ul>
  {{#files}}{{#isSource}}
  <li><p><strong><a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">Haskell Platform {{version}} for {{osNameAndArch}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isSource}}{{/files}}
</ul>
{{/current}}

<p>See the <code>README</code> file in the tarball for instructions.</p>



<hr />
<p><a href="prior.html">Prior releases</a></p>
{{> footer}}
</body>
</html>
