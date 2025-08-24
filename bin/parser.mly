%token Eof "eof"
%token<string> Identifier "identifier"
%token<string> Operator "operator"
%token<int> Integer "integer"
%token<float> Real "real"
%token<string> String "string"
%token<char> Char "char"
%token<bool> Bool "bool"

%token LParen "("
%token RParen ")"
%token LBrac "["
%token RBrac "]"
%token LCurl "{"
%token RCurl "}"

%token Dot "."
%token Colon ":"
%token Comma ","
%token Semicolon ";"
%token Equal "="
%token Bang "!"
%token Question "?"
%token Amp "&"
%token Bar "|"
%token Star "*"
%token At "@"

%token As "as"
%token Let "let"
%token Set "set"
%token In "in"
%token Fn "fn"
%token Mut "mut"
%token Sub "sub"
%token If "if"
%token Else "else"
%token While "while"
%token Loop "loop"
%token Match "match"
%token Type "type"
%token Enum "enum"
%token Struct "struct"
%token Union "union"
%token Unsafe "unsafe"
%token Const "const"
%token Mod "mod"
%token Return "return"
%token Yield "yield"
%token Break "break"
%token Continue "continue"
%token With "with"
%token Where "where"

%start main
%type<Ast.toplevel list> main
%%

let main :=
  ~ = file ; Eof ; <>

let file :=
  ~ = toplevel* ; <>

let toplevel :=
  ~ = function_definition ; <>

let function_definition :=
  "fn"
  ; name = "identifier"
  ; arguments = argument_list(arg)
  ; ":"
  ; ~ = ty
  ; "="
  ; value = expression
  ; ";"
  ; { Ast.FunctionDefinition {name ; arguments ; ty = Some ty ; value = Some value} }

let argument_list(arg) :=
| { [] }
| "|" ; args = separated_list(",", arg) ; "|" ; { args }

let mode :=
|               { Ast.value false }
| "mut"       ; { Ast.value true }
| "&"         ; { Ast.reference false }
| "&" ; "mut" ; { Ast.reference true }

let arg :=
  ~ = mode
  ; name = "identifier"
  ; key = "identifier"?
  ; default = preceded("=", expression)?
  ; ty = preceded(":", ty)
  ; { CallArgument { mode; name ; key ; ty ; default } }

let path :=
  atom = "identifier" ; <Ast.Atom>

let ty :=
  "(" ; ")"          ;  { Ast.UnitTy }
| "(" ; ~ = ty ; ")" ; <>
| ~ = path           ; <Ast.Named>

let expression :=
  "(" ; ")"                  ; { Ast.Unit }
| "(" ; ~ = expression ; ")" ; <>
| ~ = path                   ; <Ast.Variable>
| ~ = block                  ; <Ast.Block>
| ~ = "integer"              ; <Ast.Integer>

let block :=
  ~ = delimited("{", expression, "}")
  ; <Ast.Block>
