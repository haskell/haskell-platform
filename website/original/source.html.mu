<!DOCTYPE html>
<head>
    {{> header}}
    <title>Haskell Platform Source</title>
</head>
<body>
<div id="header">
<h1 class="title">Haskell Platform Source</h1>
</div>
<table>
<tbody>
<tr class="odd">
<td align="left">&lt; <a href="index.html">Home</a></td>
</tr>
</tbody>
</table>

<p>Building the Haskell Platform from source requires a working Haskell tool-chain.

<p>Download the source tarball for Unix-like systems:</p>

{{#current}}
<ul>
  {{#files}}{{#isSource}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isSource}}{{/files}}
</ul>
{{/current}}

<p>See the <code>README</code> file in the tarball for build instructions.</p>

<p> The official repository for the Haskell Platform is hosted on Github:

<pre>
<a href="https://github.com/haskell/haskell-platform">https://github.com/haskell/haskell-platform</a>
</pre>

<p><a href="prior.html">Prior releases</a></p>

<hr/>

{{> footer}}
</body>
</html>
