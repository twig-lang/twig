// Here be dragons.

%{
open Frontend
%}

%token Eof "eof"

%token Sink "_"
%token<string> Identifier "identifier"
%token<string> Operator "operator"
%token <string> Unary "unary"
%token<int> Integer "integer"
%token<float> Real "real"
%token<string> String "string"
%token<Uchar.t> Char "char"

%token LParen "("
%token RParen ")"
%token LBrac "["
%token RBrac "]"

%token Dot "."
%token Colon ":"
%token Comma ","
%token Semicolon ";"
%token Equal "="
%token Bang "!"
%token Amp "&"
%token Bar "|"
%token Star "*"
%token At "@"
%token Arrow "->"

%token As "as"
%token Let "let"
%token Set "set"
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
%token Then "then"
%token Do "do"
%token Extern "extern"
%token Import "import"
%token When "when"
%token True "true"
%token False "false"
%token Label "label"

%start main
%type<Env.variable Tree.definition list> main

%%

let main := ~ = toplevel* ; Eof ; <>
