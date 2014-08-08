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
  <li><p><strong><a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/win'); ">Haskell Platform {{version}} for {{osNameAndArch}}</a></strong>
   {{#mHash}}<br /><small>SHA-256: <code>{{mHash}}</code></small>{{/mHash}}
  </p></li>
  {{/isWindows}}{{/files}}
</ul>
{{/current}}

<hr />
<p><a href="prior.html">Prior releases</a></p>
{{> footer}}
</body>
</html>
