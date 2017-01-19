<!DOCTYPE html> <!-- -*- mode: web-mode; engine: ctemplate -*- -->
<html>
    <head>
        {{> new-site/head }}
        <link rel="stylesheet" type="text/css" href="stylesheets/contents.css">

        <meta name="description" content="Haskell Platform is a Haskell distribution with batteries included">
        <script src="js/contents.js"></script>
        <title>Haskell Platform - Included Packages</title>
    </head>
    <body class="page-home">
        <div class="wrap">
            <div class="template">
                {{> new-site/navbar }}

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
                    <h2>Prior Releases</h2>

{{#years}}
  <h3 id="section">{{year}}</h3>
    {{#releases}}
      <p><strong>{{version}}</strong>, {{month}} {{year}} ⟹
        {{#files}}
          <a href="{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">{{osNameAndArch}}</a>{{^last}} - {{/last}}
        {{/files}}
      </p>
    {{/releases}}
{{/years}}

                </div>
            </div>
        </div>

        {{> new-site/footer }}
    </body>
</html>
