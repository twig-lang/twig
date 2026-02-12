/* Miscellaneous and general things. */

%%

let parameter_name :=
  ~ = "identifier" ; <>
| "_"              ; {"_"}

/* A path... */
%public
let path :=
  ~ = "identifier" ; <Path.Atom>

| path = path
; "!"
; arguments = delimited(
    "(",
    separated_list(",", path),
    ")"
  )
; { Path.Call (path, arguments) }

/* A parameter-passing mode. */
%public
let mode :=
  is_ref = boption("&")
; is_mut = boption("mut")
; { let open Mode in
    let project = if is_ref then Projection else Value in
    let mut = if is_mut then Mutable else Immutable in
    let share = if is_ref then Reference else Data in
    Mode.create ~project ~mut ~share () }

%public
let fn_parameter :=
  name = parameter_name
; ~ = mode
; ty = preceded(":", ty)
; { Expr.Positional {mode; name; ty} }

| "*"
; name = parameter_name
; ~ = mode
; ty = preceded(":", ty)
; { Expr.Named {mode; name; ty} }

| "@"
; name = parameter_name
; ty = preceded(":", ty)
; default = preceded("=", primary)
; { Expr.Optional { name; ty; default }}

| "label"
; name = parameter_name
; ty = preceded(":", ty)
; { Expr.Label {name; ty} }

%public
let parameter_list(left, par, right) :=
  { [] }

| left
; args = separated_list(",", par)
; right
; { args }


/* The pointer mutability annotation (*const | *mut) */
%public
let ptr_mut :=
  "const" ; { Mode.Immutable }
| "mut"   ; { Mode.Mutable }

/* A yielding annotation for subscripts. */
%public
let yields :=
  "if"    ; {Ast.YieldIf}
| "while" ; {Ast.YieldWhile}
|           {Ast.YieldNone}

/* The operator token, OR a known keyword operator. */
%public
let operator :=
  ~ = "operator" ; <>
| "*"            ; {"*"}
| "|"            ; {"|"}

/* Anonymous function kinds. */
%public
let lambda_fn_kind :=
  "fn"        ; {Ast.LamFunction}
| "fn" ; "*"  ; {Ast.LamFunctionPointer}

/* Anonymous subscript kinds. */
%public
let lambda_sub_kind :=
  "sub"       ; {Ast.LamSubscript}
| "sub" ; "*" ; {Ast.LamSubscriptPointer}
