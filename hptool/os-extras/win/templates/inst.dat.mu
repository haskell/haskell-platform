{{#eachDir}}
  SetOutPath "$INSTDIR\{{dir}}"
{{#eachFile}}
  File "${FILES_SOURCE_PATH}\{{file}}"
{{/eachFile}}

{{/eachDir}}
