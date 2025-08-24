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

%type<Ast.expr> block
%type<Ast.stmt list> statements
%type<Ast.stmt> statement
%%

let main :=
  ~ = file ; Eof ; <>

let file :=
  ~ = toplevel* ; <>

let toplevel :=
  ~ = function_definition ; <>
| ~ = constant_definition ; <>

let constant_definition :=
  "const"
  ; name = "identifier"
  ; ty = preceded(":", ty)
  ; value = preceded("=", expression)
  ; ";"
  ; { Ast.ConstantDefinition { name ; ty ; value } }

let function_definition :=
  "fn"
  ; name = "identifier"
  ; arguments = argument_list(arg)
  ; ty = preceded(":", ty)?
  ; value = preceded("=", expression)?
  ; ";"
  ; { Ast.FunctionDefinition {name ; arguments ; ty ; value } }

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
  path = separated_nonempty_list(".", path_atom) ; <Ast.Member>

let path_atom :=
  ~ = "identifier" ; <Ast.Atom>

let ty :=
  "(" ; ")"          ;  { Ast.UnitTy }
| "(" ; ~ = ty ; ")" ; <>
| ~ = path           ; <Ast.Named>

let expression :=
  ~ = top_expression     ; <>
| ~ = primary_expression ; <>

let primary_expression :=
  "(" ; ")"                  ; { Ast.Unit }
| "(" ; ~ = expression ; ")" ; <>
| ~ = path                   ; <Ast.Variable>
| ~ = block                  ; <>
| ~ = "integer"              ; <Ast.Integer>

let top_expression :=
  ~ = primary_expression ; <>

let let_statement :=
  "let"
  ; name = "identifier"
  ; ty = preceded(":", ty)?
  ; "="
  ; ~ = mode
  ; value = expression
  ; { Ast.Let { name ; ty ; mode ; value } }

let block :=
  "{" ; ~ = statements ; "}" ; <Ast.Block>

let statements :=
  ~ = separated_list(";", statement) ; <>

let statement :=
  ~ = expression    ; <Ast.ExprStmt>
| ~ = let_statement ; <>
