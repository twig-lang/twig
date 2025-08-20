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

main:
  file Eof { $1 }
| Eof { [] }
;

file:
  file toplevel { $2 :: $1 }
| toplevel { [$1] }
;

toplevel:
  function_definition { $1 }
;

function_definition:
  "fn" "identifier" arglist ":" ty "=" expression ";" { Ast.FunctionDefinition {name = $2 ; arguments= $3 ; ty= Some $5 ; value = Some $7} }
;

arglist:
               { [] }
| "|" args "|" { $2 }
;

args:
 { [] }
| args "," arg { $3 :: $1 }
| arg { [$1] }
;

arg:
 "identifier" maybe_key maybe_val ":" ty { CallArgument { name = $1 ; key = $2 ; ty = $5 ; default = $3 } }
;

maybe_key: { None } | "identifier" { Some $1 };
maybe_val: { None } | "=" expression { Some $2 };

ty:
  "(" ")" { Ast.UnitTy }
;

expression:
  "(" ")" { Ast.Unit }
;
