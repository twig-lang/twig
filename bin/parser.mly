// Here be dragons.

%token Eof "eof"
%token<string> Identifier "identifier"
%token<string> Operator "operator"
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
%type<Ast.toplevel list> main

%type<Ast.expr> block

%%

let main := ~ = toplevel* ; Eof ; <>





let mod_arg :=
  name = "identifier"
; ty = preceded(":", sig_expr)?
; { Ast.ModArgModule { name ; ty }}

| "type" ; ~ = path ; <Ast.ModArgTy>

let sig_expr :=
  joins = separated_nonempty_list("&", path)
; { match joins with
    | p :: [] -> Ast.SigNamed p
    | js      -> Ast.SigJoin js }

let mod_expr :=
  ~ = delimited("(", toplevel*, ")")
; <Ast.ModBody>

| ~ = path
; <Ast.ModPath>

let import_path :=
  top = "identifier"
; sub = preceded(".", import_path)?
; { match sub with
    | Some sub -> Ast.ImportMember (top ,sub)
    | None -> Ast.ImportAtom top }

| "("
; ~ = separated_list(",", import_path)
; ")"
; <Ast.ImportMultiple>





let ty_all :=
  ~ = ty        ; <>
| ~ = struct_ty ; <>
| ~ = enum_ty   ; <>
| ~ = union_ty   ; <>

let enum_ty :=
  "enum"
; ~ = separated_list(",", enum_member)
; <Ast.TyEnum>

let enum_member :=
  name = "identifier"
; args = delimited(
  "(",
  separated_list(",", ty),
  ")"
)?
; <Ast.EnumMember>

let struct_ty :=
  "struct"
; ~ = separated_list(",", struct_member)
; <Ast.TyStruct>

let union_ty :=
  "union"
; ~ = separated_list(",", struct_member)
; <Ast.TyUnion>

let struct_member :=
  ~ = "identifier"
; ~ = preceded(":", ty)
; <Ast.StructMember>

let yields :=
  "if"    ; {Ast.YieldIf}
| "while" ; {Ast.YieldWhile}
|           {Ast.YieldNone}

let fn_name :=
  ~ = "identifier"
; <Ast.FnNamed>

| ~ = delimited("(", operator, ")")
; <Ast.FnOperator>

let parameter_list2(par, key_par) :=
  { ([], []) }
| "|"
; positional = separated_list(",", par)
; keys = preceded(";", separated_list(",", key_par))?
; "|"
; { (positional , Option.value ~default:[] keys) }

let parameter_list(par) :=
  pars = delimited("|", separated_list(",", par), "|")?
; { Option.value ~default:[] pars }

let mode :=
  is_ref = boption("&")
; is_mut = boption("mut")
; <Ast.Mode>

let fn_par :=
  ~ = mode
; ~ = "identifier"
; ~ = preceded(":", ty)
; <Ast.Parameter>

| "label"
; ~ = "identifier"
; ~ = preceded(":", ty)
; <Ast.ParameterLabel>

let key_fn_par :=
  ~ = mode
; ~ = "identifier"
; ~ = preceded("=", expression_nomsg)?
; ~ = preceded(":", ty)
; <Ast.ParameterKey>

| "label"
; ~ = "identifier"
; ty = preceded(":", ty)
; <Ast.ParameterLabel>

let path :=
  path = separated_nonempty_list(".", path_atom)
; { match path with
    | a :: xs -> List.fold_right (fun a x -> Ast.PathMember (a, x)) xs a
    | [] -> failwith "unreachable" }

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
; { Option.fold
    ~none:(Ast.PathAtom name)
    ~some:(fun a->Ast.PathCall (name, a))
    args }

let ty :=
  ~ = path           ; <Ast.TyNamed>
| "[" ; "]" ; ~ = ty ; <Ast.TySlice>
| "*" ; ~ = ptr_mut ; ~ = ty ; <Ast.TyPointer>

| ~ = delimited("[", "integer", "]")
; ~ = ty
; <Ast.TyArray>

| ts = delimited(
    "(",
    separated_list(",", ty),
    ")"
  )
; {
  match List.length ts with
  | 0 -> Ast.TyUnit
  | 1 -> List.hd ts
  | _ -> Ast.TyTuple ts }

let ptr_mut :=
  "const" ; {Ast.PtrConst}
| "mut"   ; {Ast.PtrMut}

let expr_all :=
  ~ = expression ; <>
| ~ = let_exp    ; <>
| ~ = if_exp     ; <>
| ~ = set_exp    ; <>
| ~ = while_exp  ; <>
| ~ = yield_exp  ; <>
| ~ = match_exp  ; <>
| ~ = when_exp   ; <>

| ~ = preceded("return", expr_all?) ; <Ast.ExprReturn>
| ~ = preceded("loop", expr_all) ; <Ast.ExprLoop>

| "continue" ; {Ast.ExprContinue}

| "label"
; ~ = "identifier"
; ~ = preceded(":", ty)?
; ~ = expr_all
; <Ast.ExprLabel>

| "break"
; label = "identifier"?
; value = preceded("with", expr_all)?
; <Ast.ExprBreak>

| ~ = top_all    ; <Ast.ExprTop>

let match_exp :=
  "match"
; ~ = expression_nw
; "with"
; ~ = separated_list(",", match_case)
; <Ast.ExprMatch>

let match_case :=
  ~ = pattern
; "="
; ~ = expression
; <Ast.Case>

let pattern :=
  name = path
; args = delimited("(", separated_list(",", pattern), ")")?
; { match args with
    | None -> Ast.PatNamed name
    | Some args -> Ast.PatArgs (name, args) }

let yield_exp :=
  "yield"
; ~ = mode
; ~ = expression
; <Ast.ExprYield>

let operator :=
  ~ = "operator" ; <>
| "*"            ; {"*"}
| "|"            ; {"|"}

let set_exp :=
  "set"
; l = expression
; "="
; r = expression
; <Ast.ExprSet>

let if_exp :=
  "if"
; ~ = expression
; "then"
; t = expr_all
; "else"
; f = expr_all
; <Ast.ExprIf>

| "if"; "let"
; ~ = pattern
; ~ = preceded(":", ty)?
; ~ = preceded("=", expression)
; "then"
; t = expr_all
; "else"
; f = expr_all
; <Ast.ExprIfLet>

| "if"; "match"
; ~ = pattern
; ~ = preceded(":", ty)?
; ~ = preceded("=", expression)
; "then"
; t = expr_all
; "else"
; f = expr_all
; <Ast.ExprIfMatch>

let while_exp :=
  "while"
; ~ = expression
; "do"
; ~ = expr_all
; <Ast.ExprWhile>

| "while" ; "let"
; ~ = pattern
; ~ = preceded(":", ty)?
; "="
; ~ = expression
; "do"
; ~ = expr_all
; <Ast.ExprWhileLet>

| "while" ; "match"
; ~ = pattern
; ~ = preceded(":", ty)?
; "="
; ~ = expression
; "do"
; ~ = expr_all
; <Ast.ExprWhileMatch>

let when_exp :=
  "when"
; ~ = expression
; "do"
; ~ = expr_all
; <Ast.ExprWhen>

| "when" ; "let"
; ~ = pattern
; ~ = preceded(":", ty)?
; "="
; ~ = expression
; "do"
; ~ = expr_all
; <Ast.ExprWhenLet>

| "when" ; "match"
; ~ = pattern
; ~ = preceded(":", ty)?
; "="
; ~ = expression
; "do"
; ~ = expr_all
; <Ast.ExprWhenMatch>

let expression :=
  left = expression_nw
; args = preceded("with",
    delimited("(",
      separated_list(",",key_fn_arg),
    ")")
  )?
; { match args with
    | None -> left
    | Some args ->
      let args = args
        |> List.map (fun (Ast.Argument (k,m,v)) -> (Option.get k, m, v))
      in Ast.ExprUpdate (left, args) }

let expression_nw :=
  ~ = msg_exp ; <>

| "unsafe"
; ~ = expression_nw
; <Ast.ExprUnsafe>

| ~ = delimited(
    "[",
    separated_list(",", expression),
    "]"
  )
; <Ast.ExprArray>

| "*"
; ~ = ptr_mut
; ~ = expression_nw
; <Ast.ExprAddressof>

| "@"
; ~ = mode
; ~ = expression_nw
; <Ast.ExprDeref>

let msg_exp :=
  recv = expression_nomsg
; msgs = message*
; { List.fold_left (fun r m -> Ast.ExprSend(r,m)) recv msgs }

| ~ = expression_nomsg
; ~ = preceded(":", expression_nomsg)
; <Ast.ExprTailArg>

let expression_nomsg :=
  ~ = primary
; <>

| ~ = expression_nomsg
; ~ = delimited("(", arglist ,")")
; <Ast.ExprCall>

| ~ = expression_nomsg
; ~ = delimited("[", arglist ,"]")
; <Ast.ExprSubCall>

| ~ = expression_nomsg
; "as"
; ~ = ty
; <Ast.ExprCast>

let arglist :=
  ~ = separated_list(",", fn_arg)
; <>

| positional = separated_list(",", fn_arg)
; ";"
; keys = separated_list(",", key_fn_arg)
; { List.append positional keys }

let primary :=
  ~ = path      ; <Ast.ExprVariable>
| ~ = block     ; <>
| ~ = "integer" ; <Ast.ExprInteger>
| s = "string"+ ; {Ast.ExprString (String.concat "" s)}
| ~ = "char"    ; <Ast.ExprChar>
| ~ = "real"    ; <Ast.ExprReal>
| "true"        ; {Ast.ExprBool true}
| "false"       ; {Ast.ExprBool false}

let let_exp :=
  "let"
; ~ = pattern
; ~ = preceded(":", ty)?
; "="
; ~ = mode
; ~ = expression
; <Ast.ExprLet>

let block :=
  items = delimited("(",
    separated_list(";", expr_all),
  ")")
; { match List.length items with
    | 0 -> Ast.ExprUnit
    | 1 -> List.hd items
    | _ -> Ast.ExprBlock items }

let message :=
  ~ = call_message ; <>
| ~ = op_message ; <>

let mode_exp :=
  ~ = mode
; ~ = expression_nomsg
; <>

let op_message :=
  ~ = operator
; ~ = mode_exp
; <Ast.MsgOp>

let call_message :=
  ~ = path
; ~ = delimited("(", arglist ,")")
; ~ = preceded(":", mode_exp)?
; <Ast.MsgFn>

| ~ = path
; ~ = delimited("[", arglist ,"]")
; ~ = preceded(":", mode_exp)?
; <Ast.MsgSub>

| name = path
; tail = preceded(":", mode_exp)?
; <Ast.MsgMember>

let some(x) == ~ = x ; <Some>
let none == {None}

let fn_arg :=
  ~ = none
; ~ = mode
; ~ = expression
; <Ast.Argument>

let key_fn_arg :=
  ~ = some(terminated("identifier", ":"))
; ~ = mode
; ~ = expression
; <Ast.Argument>

| name = "identifier"
; mode = mode
; { Ast.Argument (
      Some name,
      mode,
      Ast.(ExprVariable (PathAtom name))) }
