{{#eachDir}}
{{#eachFile}}
  Delete "$INSTDIR\{{file}}"
{{/eachFile}}
  RMDir "$INSTDIR\{{dir}}"

{{/eachDir}}
