<!DOCTYPE html> <!-- -*- mode: web-mode; engine: ctemplate -*- -->
<html>
    <head>
        {{> plan-a/head }}
        <link rel="stylesheet" type="text/css" href="stylesheets/download.css">

        <meta name="description" content="Haskell Platform is a Haskell distribution with batteries included">
        <script src="js/download.js"></script>
        <script>var preferredOS = "linux";</script>
        <title>Download Haskell Platform For Linux</title>
    </head>
    <body class="page-home">
        <div class="wrap">
            <div class="template">
                {{> plan-a/navbar }}
                {{> plan-a/download-banner.html }}
                <!-- do not include the "download options" section -->
                {{> plan-a/download-os-sections.html }}
            </div>
        </div>

        <!-- a bit of whitespace before the footer -->
        <div style="height: 100px;"></div>

        {{> plan-a/footer }}
    </body>
</html>
