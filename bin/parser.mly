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
%token Then "then"
%token Do "do"
%token Extern "extern"

%start main
%type<Ast.toplevel list> main

%type<Ast.expr> block

%%

let main :=
  ~ = file ; Eof ; <>

let file :=
  ~ = toplevel* ; <>

let toplevel :=
  ~ = function_definition ; <>
| ~ = constant_definition ; <>
| ~ = type_definition     ; <>
| ~ = extern              ; <>

let extern :=
  "extern"
  ; abi = "string"?
  ; "fn"
  ; name = "identifier"
  ; parameters = parameter_list(fn_par)
  ; ty = preceded(":", ty)?
  ; ";"
  ; { Ast.Extern { abi ; name ; parameters ; ty } }

let type_definition :=
  "type"
  ; name = "identifier"
  ; "="
  ; ty = ty_all
  ; ";"
  ; { Ast.TypeDefinition { name ; ty } }

let ty_all :=
  ~ = ty        ; <>
| ~ = struct_ty ; <>
| ~ = enum_ty   ; <>
| ~ = union_ty   ; <>

let enum_ty :=
  "enum"
  ; members = delimited(
    "(",
    separated_list(",", enum_member),
    ")"
  )
  ; { Ast.EnumTy { members } }

let enum_member :=
  name = "identifier"
  ; args = delimited(
    "(",
    separated_list(",", ty),
    ")"
  )?
  ; {
  match args with
  | None -> Ast.EnumMember { name }
  | Some args -> Ast.EnumMemberArgs { name ; args }
  }

let struct_ty :=
  "struct"
  ; members = delimited(
   "(",
   separated_list(",", struct_member),
   ")"
  )
  ; { Ast.StructTy { members } }

let union_ty :=
  "union"
  ; members = delimited(
   "(",
   separated_list(",", struct_member),
   ")"
  )
  ; { Ast.UnionTy { members } }

let struct_member :=
  name = "identifier"
  ; ":"
  ; ty = ty
  ; { Ast.StructMember { name ; ty } }

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
  ; parameters = parameter_list(fn_par)
  ; ty = preceded(":", ty)?
  ; value = preceded("=", expr_all)?
  ; ";"
  ; { Ast.FunctionDefinition {name ; parameters ; ty ; value } }

let parameter_list(par) :=
  pars = delimited("|", separated_list(",", fn_par), "|")?
  ; { Option.value ~default:[] pars }

let mode :=
  is_ref = boption("&")
  ; is_mut = boption("mut")
  ; { Ast.Mode { is_ref ; is_mut} }

let fn_par :=
  ~ = mode
  ; name = "identifier"
  ; key = "identifier"?
  ; default = preceded("=", expression_nomsg)?
  ; ty = preceded(":", ty)
  ; { FnParameter { mode; name ; key ; ty ; default } }

let path :=
  path = separated_nonempty_list(".", path_atom) ; <Ast.Member>

let path_atom :=
  name = "identifier"
  ; args = preceded(
    "!",
    delimited(
      "(",
      separated_list(",", path),
      ")"
    )
  )?
  ; {
   match args with
   | None -> Ast.Atom name
   | Some args -> Ast.Call { name ; args }
  }

let ty :=
  ts = delimited(
    "(",
    separated_list(",", ty),
    ")"
  )
  ; {
   match List.length ts with
   | 0 -> Ast.UnitTy
   | 1 -> List.hd ts
   | _ -> Ast.Tuple ts
  }
| ~ = path           ; <Ast.Named>

let expr_all :=
  ~ = msg_exp   ; <>
| ~ = let_exp   ; <>
| ~ = if_exp    ; <>
| ~ = set_exp   ; <>
| ~ = while_exp ; <>

let operator :=
  ~ = "operator" ; <>
| "*"            ; {"*"}
| "|"            ; {"|"}

let set_exp :=
  "set"
  ; lval = expression
  ; "="
  ; operator = operator?
  ; rval = expression
  ; { Ast.Set { lval ; operator; rval } }

let if_exp :=
  "if"
  ; condition = expression
  ; "then"
  ; taken = expression
  ; "else"
  ; not_taken = expression
  ; { Ast.If { condition ; taken ; not_taken } }

let while_exp :=
  "while"
  ; condition = expression
  ; "do"
  ; body = expression
  ; { Ast.While { condition ; body } }

let expression :=
  "unsafe" ; ~ = expression ; <Ast.Unsafe>
| ~ = msg_exp ; <>

let msg_exp :=
  recv = expression_nomsg
  ; msgs = message*
  ; { List.fold_left (fun r m -> Ast.Send { recv = r ; msg = m } ) recv msgs }

let expression_nomsg :=
  ~ = call ; <>

let call :=
  callee = primary
  ; args = delimited("(", separated_list(",", fn_arg) ,")")*
  ; {
   List.fold_left (fun c a -> Ast.FnCall { callee = c ; args = a } ) callee args
  }

let primary :=
  ~ = path                   ; <Ast.Variable>
| ~ = block                  ; <>
| ~ = "integer"              ; <Ast.Integer>

let let_exp :=
  "let"
  ; name = "identifier"
  ; ty = preceded(":", ty)?
  ; "="
  ; ~ = mode
  ; value = expression
  ; { Ast.Let { name ; ty ; mode ; value } }

let block :=
  "("
  ; items = separated_list(";", expr_all)
  ; ")"
  ; { match List.length items with
      | 0 -> Ast.Unit
      | 1 -> List.hd items
      | _ -> Ast.Block items }

let message :=
  ~ = fn_message ; <>
| ~ = op_message ; <>

let op_message :=
  name = operator
  ; arg = expression_nomsg
  ; { Ast.OpMessage { name ; arg } }

let fn_message :=
  name = path
  ; args = delimited("(", separated_list(",", fn_arg) ,")")?
  ; tail = preceded(":", expression_nomsg)?
  ; { Ast.FnMessage {
    name ;
    args = Option.value ~default:[] args ;
    tail }  }

let fn_arg :=
  key = preceded(":","identifier")?
  ; mode = mode
  ; value = expression
  ; { Ast.FnArgument { key ; mode ; value } }
