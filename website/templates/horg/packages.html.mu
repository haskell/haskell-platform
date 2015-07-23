<div class="release-notes">
<p>
    Along with a compiler and basic development environment, Haskell Platform provides
    a variety of widely-used packages from <a href="http://hackage.haskell.org/">Hackage</a>, the
    Haskell package repository.
</p>
<p>
    If you are a library author and would like to see your
    package included in Haskell Platform, see the
    <a href="https://github.com/haskell/haskell-platform/wiki/AddingPackages">Haskell Platform Wiki</a>
    for the package proposal procedure.
</p>
</div>

<div>
    <label for="package-filter">Package name:</label>
    <input id="package-filter" placeholder="Filter by package name">
</div>

{{#history}}
<table class="history table">
    <thead>
        <tr>
            <th>&nbsp;</th>
            {{#hpReleases}}
            <th class="version">{{hpVersion}}</th>
            {{/hpReleases}}
        </tr>
    </thead>

    {{#sections}}
    <tbody>
        <tr class="section-header"><th colspan="{{ncols}}">{{name}}</th></tr>

        {{#components}}
        <tr class="package">
            <td class="package"><a href="{{hackageUrl}}">{{package}}</a></td>
            {{#releases}}
            <td class="version {{class}}">{{version}}</td>
            {{/releases}}
        </tr>
        {{/components}}
    </tbody>
    {{/sections}}
</table>
{{/history}}

