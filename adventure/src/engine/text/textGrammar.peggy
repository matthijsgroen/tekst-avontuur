
FormattedText
  = TextNode * 
  
Text "text"
  = [^{[]+ { return { text: text(), type: "text" } }

FormattingKey "formatKey"
  = [a-z]+ { return text() }
  
FormattingValue "formatValue"
  = [^}\]]+ { return text() }

Formatting "formatting"
  = "{" key:FormattingKey value: ( "=" value:FormattingValue ) ? "}" text:FormattedText "{/" FormattingKey "}" { return { type: "formatting", format: key, value: value ? value[1]: null, contents: text } }

Interpolation "interpolation"
  = "[" key:FormattingValue "]" { return { type: "interpolation", value: key } }

TextNode "formatted text"
  = Text
  / Formatting
  / Interpolation
