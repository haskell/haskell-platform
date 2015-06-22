<!DOCTYPE html>
<html>
  <head>
    {{> horg/header}}
    <title>Haskell Platform for Linux</title>
  </head>
  <body class='page-downloads'>
    {{> horg/topbody}}


<div class="container"><ol class="breadcrumb">  <li><a href="hp.html">Haskell Platform</a>  <li><a href="#">Source</a></ol></div>


<div class="container">
  <div class="row">
    <div class="span12 col-md-12">

<h2>Haskell Platform Source</h2>

<p>Building the Haskell Platform from source requires a working Haskell tool-chain.

<p>Download the source tarball for Unix-like systems:</p>

<div class="downloads">
{{#current}}
<ul>
  {{#files}}{{#isSource}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{downloadsUrl}}{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isSource}}{{/files}}
</ul>
{{/current}}
</div>

<p>See the <code>README</code> file in the tarball for build instructions.</p>

<p> The official repository for the Haskell Platform is hosted on Github:

<p><code>
<a href="https://github.com/haskell/haskell-platform">https://github.com/haskell/haskell-platform</a>
</code>

<p><a href="prior.html">Prior releases →</a></p>

    </div>
  </div>
</div>

    {{> horg/footer}}
    {{> horg/epilogue}}
  </body>
</html>
