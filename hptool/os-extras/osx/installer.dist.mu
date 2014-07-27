<?xml version="1.0" encoding="utf-8" standalone="no"?>
<installer-gui-script minSpecVersion="1">
    <options customize="never"
        hostArchitectures="i386"
        require-scripts="true"
        />

    <title>Haskell Platform {{hpVersion}}</title>
    <background file="logo-color.png" mime-type="image/png"
        alignment="bottomleft"/>

    <welcome file="welcome.rtf" mime-type="text/rtf"/>
    <!-- <license file="license.rtf" mime-type="text/rtf"/>  -->
    <!-- <readme file="readme.rtf" mime-type="text/rtf"/>  -->
    <!-- <conclusion file="conclusion.rtf" mime-type="text/rtf"/>  -->

    <choices-outline>
        <line choice="default">
            <line choice="GHC.choice"/>
            <line choice="Libraries.choice"/>
        </line>
    </choices-outline>

    <choice id="default"/>
    <choice id="GHC.choice" visible="false">
        <pkg-ref id="GHC.pkg-ref"/>
    </choice>
    <choice id="Libraries.choice" visible="false">
        <pkg-ref id="Libraries.pkg-ref"/>
    </choice>

    <pkg-ref id="GHC.pkg-ref">GHC.pkg</pkg-ref>
    <pkg-ref id="Libraries.pkg-ref">HaskellPlatform.pkg</pkg-ref>

    <installation-check script="find_gcc()"/>
    <volume-check script="true">
        <allowed-os-versions>
            <os-version min="10.6"/>
        </allowed-os-versions>
    </volume-check>

    <script>
<![CDATA[
function find_gcc() {
  var r = system.files.fileExistsAtPath('/usr/bin/gcc');

  if (!r) {
    my.result.type = 'Warn';
    my.result.title = 'Command Line Build Tools Required';
    my.result.message = 'The command line build tools (C compiler, linker, etc...) need to be installed. They are an optional part of Xcode, or can be installed independently. See http://hackage.haskell.org/platform/mac.html for details.';
  }

  return r;
}
]]>
    </script>

</installer-gui-script>


