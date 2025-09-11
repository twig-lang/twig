/* Expressions */

%%

/* Expressions AND statements. */
%public
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
; ~ = preceded("->", ty_sink)?
; ~ = expr_all
; <Ast.ExprLabel>

| "break"
; label = "identifier"?
; value = preceded("with", expr_all)?
; <Ast.ExprBreak>

let let_exp :=
  "let"
; ~ = pattern
; ~ = preceded(":", ty)?
; "="
; ~ = mode
; ~ = expression
; <Ast.ExprLet>

| "let"
; ~ = top_definition
; <Ast.ExprTop>

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

| "_" ; {Ast.PatSink}

let yield_exp :=
  "yield"
; ~ = mode
; ~ = expression
; <Ast.ExprYield>

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

/* Message sends and `value with (...)` */
%public
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

/* Expressions not including `with`, trailing arguments,
   nor message sends. */
%public
let expression_nomsg :=
  ~ = primary ; <>

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

/* A "primitive" expression. */
%public
let primary :=
  ~ = path      ; <Ast.ExprVariable>
| ~ = block     ; <>
| ~ = anon_fn   ; <>
| ~ = "integer" ; <Ast.ExprInteger>
| s = "string"+ ; {Ast.ExprString (String.concat "" s)}
| ~ = "char"    ; <Ast.ExprChar>
| ~ = "real"    ; <Ast.ExprReal>
| "true"        ; {Ast.ExprBool true}
| "false"       ; {Ast.ExprBool false}

let anon_fn :=
  ~ = lambda_fn_kind
; ~ = parameter_list2("(", fn_par, key_fn_par, ")")
; ~ = preceded("->", ty)?
; ~ = preceded("=", primary)
; <Ast.ExprLambda>

| ~ = lambda_sub_kind
; ~ = parameter_list2("[", fn_par, key_fn_par, "]")
; ~ = preceded("->", ty)?
; ~ = preceded("=", primary)
; <Ast.ExprLambda>

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
