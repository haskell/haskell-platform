<!DOCTYPE html>
<html>
  <head>
    {{> horg/header}}
    <title>Haskell Platform for Windows</title>
  </head>
  <body class='page-downloads'>
    {{> horg/topbody}}


<div class="container"><ol class="breadcrumb">  <li><a href="hp.html">Haskell Platform</a>  <li><a href="#">Windows</a></ol></div>


<div class="container">
  <div class="row">
    <div class="span12 col-md-12">


<h2>Haskell Platform for Windows</h2>

<div class="downloads">
{{#current}}
<ul>
  {{#files}}{{#isWindows}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{downloadsUrl}}{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/win'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isWindows}}{{/files}}
</ul>
{{/current}}
</div>

<p><a href="prior.html">Prior releases →</a></p>

<hr />
<div class="release-notes">
<h4>Release Notes:</h4>

Check your PATH variable after installation to be sure it is what you expect -
especially if you have another version of the Haskell Platform installed or
have manually installed Haskell in the past.
</div>


    </div>
  </div>
</div>

    {{> horg/footer}}
    {{> horg/epilogue}}
  </body>
</html>
