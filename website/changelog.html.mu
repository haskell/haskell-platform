<!DOCTYPE html>
<head>
    {{> header}}
    <title>The Haskell Platform: Changelog</title>
</head>
<body>
<div id="header">
<h1 class="title">The Haskell Platform: Changelog</h1>
</div>
<h2 id="changelog-for-the-haskell-platform">Package Vesions by Release</h2>
<p>Versions of each package included in the Platform, by release of the
Platform. Versions that are a new major revision for a given release of the
platform are in black. Those that are the same, or just a minor revision are
in grey.</p>

{{#history}}
  <table class="history">
  <tr>
    <td>&nbsp;</td>
    {{#hpReleases}}
      <td class="version">{{hpVersion}}</td>
    {{/hpReleases}}
  </tr>
  {{#sections}}
    <tr><td class="section" colspan="{{ncols}}">{{name}}</td></tr>
    {{#components}}
      <tr class="packageRow">
        <td class="package"><a href="{{hackageUrl}}">{{package}}</td>
        {{#releases}}
          <td class="version {{class}}">{{version}}</td>
        {{/releases}}
      </tr>
    {{/components}}
  {{/sections}}
  </table>
{{/history}}

{{> footer}}
</body>
</html>
