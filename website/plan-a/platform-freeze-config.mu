-- Config file for Haskell Platform {{#current}}{{version}}, {{month}} {{year}}{{/current}}

constraints: {{#freezeConfig}} {{^first}}
             {{/first}}{{name}} == {{version}}{{^last}},{{/last}}
{{/freezeConfig}}
