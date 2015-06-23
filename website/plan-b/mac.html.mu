<!DOCTYPE html>
<html>
  <head>
    {{> horg/header}}
    <title>Haskell Platform for Mac OS X</title>
  </head>
  <body class='page-downloads'>
    {{> horg/topbody}}


<div class="container"><ol class="breadcrumb">  <li><a href="hp.html">Haskell Platform</a>  <li><a href="#">Mac OS X</a></ol></div>


<div class="container">
  <div class="row">
    <div class="span12 col-md-12">

<h2>Haskell Platform for Mac OS X</h2>

<div class="downloads">
{{#current}}
<ul>
  {{#files}}{{#isOSX}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{> downloads-root}}{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/mac'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isOSX}}{{/files}}
</ul>
{{/current}}
</div>

<p><em>For Mac OS X 10.5 please use the 2010.2.0.0 installer.</em></p>

<p><a href="prior.html">Prior releases →</a></p>

<hr />
<div class="release-notes">
<h4>Release Notes:</h4>

<p><em>The command line development tools are required prior to installation.</em> <br />If you have <tt>/usr/bin/ld</tt> available in a shell, you should be good to go.
<p> If not:
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

<hr />
</div>

    </div>
  </div>
</div>

    {{> horg/footer}}
    {{> horg/epilogue}}
  </body>
</html>
