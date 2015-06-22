<!DOCTYPE html>
<html>
  <head>
    {{> horg/header}}
    <title>Haskell Platform for Linux</title>
  </head>
  <body class='page-downloads'>
    {{> horg/topbody}}


<div class="container"><ol class="breadcrumb">  <li><a href="hp.html">Haskell Platform</a>  <li><a href="#">Linux</a></ol></div>


<div class="container">
  <div class="row">
    <div class="span12 col-md-12">

<h2>Haskell Platform for Linux</h2>
<p>The Haskell Platform for Linux should work on any modern Linux system.

<div class="downloads">
{{#current}}
<ul>
  {{#files}}{{#isLinux}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{downloadsUrl}}{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/linux'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isLinux}}{{/files}}
</ul>
{{/current}}
</div>

<p>Success reported with</b> Ubuntu 12.02, Debian 7, Fedora 20, openSUSE 13

<p><a href="prior.html">Prior releases →</a></p>

<div class="release-notes">
<p>To install the .tar.gz archive, run:</p>
<pre><code>tar xvf ...downloaded-tar-file...
sudo ./install-haskell-platform.sh</code>
</pre>

<p>
The Haskell Platform requires the <code>libz</code>, <code>libgmp10</code>, the <code>freeglut3</code> libraries and development tools.

<p>To install these components run the following shell commands:

<p> on Debian / Ubuntu systems:
<pre><code>apt-get install -y build-essential zlib1g-dev libgmp10-dev freeglut3</code>
</pre>
<p> on RPM-based systems:
<pre><code>yum groupinstall 'Development Tools'
yum install -y gmp-devel zlib-devel freeglut</code>
</pre>

    </div>
  </div>
</div>

    {{> horg/footer}}
    {{> horg/epilogue}}
  </body>
</html>
