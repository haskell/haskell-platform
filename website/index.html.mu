<!DOCTYPE html>
<html>
  <head>
      {{> header}}
      <title>Download Haskell</title>
  </head>
  <body id="index" class="spring">

    <!-- Download box !-->

    <div id="download_bar">
      <div class="wrap">

       <h1>The Haskell Platform</h1>

           <img id="logo" src="images/download-winter.png" alt="Haskell homepage">
       <br />

        <div class="platforms">
            <table align="center" width="290px">
                <tr valign="bottom">
                    <td>
            <a href="windows.html"><img id="windows"
                src="images/windows.png" alt="For Windows"></a> <br /><a
                href="windows.html">Windows</a>
                    </td>
                    <td>
            <a href="mac.html"    ><img id="osx"
                src="images/OS_X-Logo.png" alt="For Mac OSX"></a> <br
            /><a href="mac.html">Mac</a>
                    </td>
                    <td>
            <a href="linux.html"  ><img id="linux"
                src="images/Tux.png"     alt="For Linux"></a> <br /><a
                href="linux.html">Linux</a>
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
            <b>New packages</b>
            <br />attoparsec
            <br />case-insensitive,
            <br />hashable
            <br />unordered-containers
        </p>
        <p>
            <b>Major update</b>
            <br />OpenGL and GLUT
        </p>
        <p>
            Next release: <a href="http://trac.haskell.org/haskell-platform/wiki/ReleaseTimetable">2013.4.0.0</a>
            <br />
            <a href="prior.html">Prior releases</a>
        </p>
        <p>
            <a href="http://haskell.org/haskellwiki/Haskell_Platform">Problems?</a>
            <br />
            <a href="doc/2013.2.0.0/start.html">Documentation</a>
            <br />
            <a href="doc/2013.2.0.0/frames.html">Library Doc</a>
        </p>
    </div>


    {{> footer}}
  </body>
</html>
