<!DOCTYPE html>
<head>
    {{> header}}
    <title>Haskell Platform Prior Versions</title>
</head>
<body>
<div id="header">
<h1 class="title">Haskell Platform Prior Versions</h1>
</div>
<table>
<tbody>
<tr class="odd">
<td align="left">&lt; <a href="index.html">Home</a></td>
</tr>
</tbody>
</table>

<p>Prior releases of Haskell Platform:</p>
{{#years}}
  <h1 id="section">{{year}}</h1>
    {{#releases}}
      <p><strong>{{version}}</strong>, {{month}} {{year}} ⟹
        {{#files}}
          <a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">{{osNameAndArch}}</a> -
        {{/files}}
      </p>
    {{/releases}}
{{/years}}

{{> footer}}
</body>
</html>
