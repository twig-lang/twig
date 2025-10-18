/* Miscellaneous and general things. */

%%

let parameter_name :=
  ~ = "identifier" ; <>
| "_"              ; {"_"}

/* Function parameters. */
%public
let fn_par :=
  ~ = mode
; ~ = parameter_name
; ~ = preceded(":", ty)
; <Expr.PPValue>

| "label"
; ~ = parameter_name
; ~ = preceded(":", ty)
; <Expr.PPLabel>

/* Key function parameters. Note the optional default value. */
%public
let key_fn_par :=
  mode = mode
; name = parameter_name
; value = preceded("=", primary)?
; ty = preceded(":", ty)
; { match value with
    | Some value -> Expr.PNKey (mode, name, ty, value)
    | None -> Expr.PNValue (mode, name, ty) }

| "label"
; ~ = parameter_name
; ty = preceded(":", ty)
; <Expr.PNLabel>

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
; { let arguments = List.map (fun x -> Path.Argument x) arguments in
    Path.Call (path, arguments) }

| ~ = path
; ~ = preceded(".", "identifier")
; <Path.Member>

/* A parameter-passing mode. */
%public
let mode :=
  is_ref = boption("&")
; is_mut = boption("mut")
; { let open Mode in
    let m = if is_mut then Mutable else Immutable in
    let r = if is_ref then Reference else Value in
    Mode.Mode (m, r) }

/* A list of parameters */
%public
let parameter_list(left, par, right) :=
  ~ = delimited(left, separated_list(",", par), right)
; <>

/* Same as `parameter_list`, but uses ';' to separate between positional and
   named parameters. */
%public
let parameter_list2(left, par, key_par, right) :=
  { ([], []) }

| left
; positional = separated_list(",", par)
; keys = preceded(";", separated_list(",", key_par))?
; right
; { (positional , Option.value ~default:[] keys) }

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
