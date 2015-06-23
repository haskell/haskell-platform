<!DOCTYPE html>
<html>
  <head>
    {{> horg/header}}
    <title>Haskell Platform for Linux</title>
  </head>
  <body class='page-downloads'>
    {{> horg/topbody}}


<div class="container"><ol class="breadcrumb">  <li><a href="hp.html">Haskell Platform</a>  <li><a href="#">Prior Releases</a></ol></div>


<div class="container">
  <div class="row">
    <div class="span12 col-md-12">

<h2>Haskell Platform Prior Releases</h2>


{{#years}}
  <h3 id="section">{{year}}</h3>
    {{#releases}}
      <p><strong>{{version}}</strong>, {{month}} {{year}} ⟹
        {{#files}}
          <a href="{{> downloads-root}}{{url}}" onClick="javascript: pageTracker._trackPageview('/downloads/mac/old'); ">{{osNameAndArch}}</a>{{^last}} - {{/last}}
        {{/files}}
      </p>
    {{/releases}}
{{/years}}


    </div>
  </div>
</div>

    {{> horg/footer}}
    {{> horg/epilogue}}
  </body>
</html>
