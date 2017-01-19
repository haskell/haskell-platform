<!DOCTYPE html> <!-- -*- mode: web-mode; engine: ctemplate -*- -->
<html>
    <head>
        {{> new-site/head }}
        <link rel="stylesheet" type="text/css" href="stylesheets/download.css">

        <meta name="description" content="Haskell Platform is a Haskell distribution with batteries included">
        <script src="js/download.js"></script>
        <title>Download Haskell Platform</title>
    </head>
    <body class="page-home">
        <div class="wrap">
            <div class="template">
                {{> new-site/navbar }}
                {{> new-site/download-banner.html }}
                <!-- do not include the "download options" section -->
                {{> new-site/download-os-sections.html }}
            </div>
        </div>

        <!-- a bit of whitespace before the footer -->
        <div style="height: 100px;"></div>

        {{> new-site/footer }}
    </body>
</html>
