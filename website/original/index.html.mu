<!DOCTYPE html>
<html>
  <head>
      {{> header}}
      <title>Download Haskell</title>
      <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font
-awesome.min.css">
  </head>
  <body id="index" class="summer">

    <!-- Download box !-->

    <div id="download_bar">
      <div class="wrap">

       <h1>The Haskell Platform</h1>

           <img id="logo" src="images/download-summer.png" alt="Haskell homepage">
       <br />

        <div class="platforms">
            <table align="center" width="440">
                <tr valign="bottom">
                    <td>
            <a href="windows.html"><img id="windows"
                src="images/windows.png" alt="For Windows"></a> <br /><a
                href="windows.html">Windows</a>
                    </td>
                    <td>
            <a href="mac.html"    ><img id="osx"
                src="images/OS_X-Logo.png" alt="For Mac OSX"></a> <br
            /><a href="mac.html">Mac OSX</a>
                    </td>
                    <td>
            <a href="linux.html"  ><img id="linux"
                src="images/Tux.png"     alt="For Linux"></a> <br /><a
                href="linux.html">Linux</a>
                    </td>
                    <td>
            <span class="source-logo">
            <a href="source.html">
            <i class="fa fa-code source-pad"></i>
            </a>
            </span>
            <br/><a href="source.html">Source</a>
                    </td>
                </tr>
            </table>
        </div>

      </div>
    </div>

    <!-- Footer !-->

    <div id="info">

        <table>
            <tr>
                <td valign="top">

        <div class="col1">
            <div class="title">
            Comprehensive
            </div>
            <div class="points">
                The Haskell Platform is the easiest way to get started
                with programming Haskell. It comes with all you need to get
                up and running. Think of it
                as "Haskell: batteries included".
        <a href="contents.html">Learn more...</a>
            </div>
        </div>

                </td>
                <td valign="top">

        <div class="col2">
            <div class="title">
                Robust
            </div>
            <div class="points">
                The Haskell Platform contains only stable and
                widely-used tools and libraries, drawn from
                a pool of thousands of <a
                    href="http://hackage.haskell.org">Haskell
                    packages</a>, ensuring you get the best from what
                is on offer.
            </div>
        </div>

                </td>
                <td valign="top">

        <div class="col3">
            <div class="title">
            Cutting Edge
            </div>
            <div class="points">
                The Haskell Platform ships with advanced features such
                as multicore parallelism, thread sparks and
                transactional memory, along with
        <a href="contents.html#functionality">many other
            technologies</a>, to help you get work done.
            </div>
        </div>
                </td>

            </tr>
        </table>

    </div>

     <div id="timeline">
        <p>
            Current release: <a href="changelog.html">{{hpVersion}}</a>
        </p>
        <p>
            <b>New GHC:</b> {{ghcVersion}}
            <!-- <br/><b>Major update:</b>OpenGL and GLUT -->
            <br/><a href="changelog.html">Included Packages</a>
        </p>
        <p>
            <a href="prior.html">Prior releases</a><br />
            <a href="https://github.com/haskell/haskell-platform/wiki/ReleaseTimetable">Future schedule</a>
        </p>
        <p>
            <a href="http://haskell.org/haskellwiki/Haskell_Platform">Problems?</a>
            <br />
            <a href="doc/{{hpVersion}}/start.html">Documentation</a>
            <br />
            <a href="doc/{{hpVersion}}/platform/doc/frames.html">Library Doc</a>
        </p>
    </div>


    {{> footer}}
  </body>
</html>
