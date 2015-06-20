<!DOCTYPE html>
<head>
    {{> header}}
    <title>Haskell Platform for Windows</title>
</head>
<body>
<div id="header">
<h1 class="title">Haskell Platform for Windows</h1>
</div>
<table>
<tbody>
<tr class="odd">
<td align="left">&lt; <a href="index.html">Home</a></td>
</tr>
</tbody>
</table>
<p><img style="float:right;" src="images/windows.png" /> Download the Windows installer here:</p>

{{#current}}
<ul>
  {{#files}}{{#isWindows}}
  <li><p><strong>{{osNameAndArch}}: <a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/win'); ">Haskell Platform {{version}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isWindows}}{{/files}}
</ul>
{{/current}}

<p><a href="prior.html">Prior releases</a></p>

<hr />
<h2>Release Notes:</h2>
<ul class="notes">
  <li>Check your PATH variable after installation to be sure it is what you expect - 
especially if you have another version of the Haskell Platform installed or
have manually installed Haskell in the past.
</ul>

<hr />
{{> footer}}
</body>
</html>
