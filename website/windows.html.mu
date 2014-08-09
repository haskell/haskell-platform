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
<h2>Release Notes:</h2>
<ul>
    <li>Windows release now comes in 32bit and 64bit versions.</li>
    <li>If you have other Haskell Platform installations or had hand-installed
        Haskell in the past, please check your PATH variable after the
        installation and be sure it is what your expect.</li>
    <li>Using ghci to build an executable that links against a DLL may result in
        numerous warnings about symbols (GHC ticket #9297)</li>
    <li>This distribution includes GHC 7.8.3, which in turn includes MinGW.</li>
    <li>This distribution includes an updated version of the OpenGL Utility
        Toolkit (GLUT) from the <a href="http://www.transmissionzero.co.uk/software/freeglut-devel/">FreeGLUT project</a></li>
    <li>Due to how GHC works on Windows, the platform libraries are not built
        split-objs, don't use dynamic libraries, and don't use LLVM.</li>
</ul>

<hr />
<p><a href="prior.html">Prior releases</a></p>
{{> footer}}
</body>
</html>
