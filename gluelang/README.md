# Glue Language Specification

The Glue Language is a domain-specific language, that abstracts away a data model.
It aims to be something like [Protocol Buffers](https://developers.google.com/protocol-buffers/), but with a more concise syntax,
and allowing for binary encoding as an opt-in feature.

The language can be described in the below EBNF:

```
(* Top level structure *)
Program = VersionDeclaration NewLine ModelDefinition ;

(* Version declaration *)
VersionDeclaration = "@version" WhiteSpace "=" WhiteSpace StringLiteral ;

(* Model definition *)
ModelDefinition = DocumentationComment NewLine "model" WhiteSpace Identifier WhiteSpace ModelBody ;

(* Model body *)
ModelBody = "{" NewLine {ModelField} "}" ;

(* Model field *)
ModelField = WhiteSpace DocumentationComment NewLine WhiteSpace [InternalComment NewLine WhiteSpace] Type WhiteSpace Identifier ["?"] "," NewLine ;

(* Documentation comment *)
DocumentationComment = "///" WhiteSpace TextContent ;

(* Internal comment *)
InternalComment = "//" WhiteSpace "(" TextContent ")" ;

(* Basic types *)
Type = "string" | "number" ;

(* Auxiliary definitions *)
Identifier = Letter {Letter | Digit | "_"} ;
StringLiteral = '"' {Character} '"' ;
TextContent = {Character} ;
Letter = "A" | "B" | "C" | ... | "Z" | "a" | "b" | "c" | ... | "z" ;
Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
Character = Letter | Digit | SpecialChar ;
WhiteSpace = " " {" "} ;
NewLine = "\n" {"\n"} ;
SpecialChar = ":" | "-" | "." | "," | "(" | ")" | ... ;
```
