<!DOCTYPE html> <!-- -*- mode: web-mode; engine: ctemplate -*- -->
<html>
    <head>
        {{> head }}
        <link rel="stylesheet" type="text/css" href="stylesheets/contents.css">

        <meta name="description" content="Haskell Platform is a Haskell distribution with batteries included">
        <script src="js/contents.js"></script>
        <title>Haskell Platform - Included Packages</title>
    </head>
    <body class="page-home">
        <div class="wrap">
            <div class="template">
                {{> navbar }}

                <div class="header">
                    <div class="container">
                        <div class="row">
                            <div class="span12 col-md-12">
                                <div class="branding">
                                    <span style="background-image: url(img/logo.png)" class="name">Haskell Platform</span>
                                    <span class="summary">
                                        Haskell with batteries included
                                    </span>
                                </div>
                            </div>
                            <div class="span6 col-md-6">
                                <div class="branding">
                                    <span class="tag"></span>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="container">
                    <h2>Included Packages</h2>
                    <p>
                        Along with a compiler and basic development environment, Haskell Platform provides
                        a variety of widely-used packages from <a href="http://hackage.haskell.org/">Hackage</a>, the
                        Haskell package repository.
                    </p>
                    <p>
                        If you are a library author and would like to see your
                        package included in Haskell Platform, see the
                        <a href="https://github.com/haskell/haskell-platform/wiki/AddingPackages">Wiki</a>
                        for the package proposal procedure.
                    </p>

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
                </div>
            </div>
        </div>

        {{> footer }}
    </body>
</html>
