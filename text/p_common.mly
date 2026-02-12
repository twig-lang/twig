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
; <Expr.Positional_value>

| "label"
; ~ = parameter_name
; ~ = preceded(":", ty)
; <Expr.Positional_label>

/* Key function parameters. Note the optional default value. */
%public
let key_fn_par :=
  mode = mode
; name = parameter_name
; value = preceded("=", primary)?
; ty = preceded(":", ty)
; { match value with
    | Some value -> name, Expr.Named_key (mode, ty, value)
    | None -> name, Expr.Named_value (mode, ty) }
| "label"
; name = parameter_name
; ty = preceded(":", ty)
; { name, Expr.Named_label ty }

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

/* A list of parameters */
%public
let parameter_list(left, par, right) :=
  ~ = delimited(left, separated_list(",", par), right)
; <>

let new_function_parameter :=
  ~ = mode
; name = parameter_name
; ty = preceded(":", ty)
; { Expr.Positional {mode; name; ty} }

| "*"
; ~ = mode
; name = parameter_name
; ty = preceded(":", ty)
; { Expr.Named {mode; name; ty} }

| name = parameter_name
; ty = preceded(":", ty)
; default = preceded("=", primary)
; { Expr.Optional { name; ty; default }}

| "label"
; name = parameter_name
; ty = preceded(":", ty)
; { Expr.Label {name; ty} }

let function_parameter :=
  "label"
; name = parameter_name
; ty = preceded(":", ty)
; {
  Either.right @@ (name, Expr.Named_label (ty))
}

| ~ = mode
; name = parameter_name
; key  = parameter_name?
; ty = preceded(":", ty)
; {
  match key with
  | Some key -> Either.right @@ (key, Expr.Named_value (mode, ty))
  | None -> Either.left @@ (Expr.Positional_value (mode, name, ty))
}

/* Same as `parameter_list`, but uses ';' to separate between positional and
   named parameters. */
%public
let parameter_list2(left, _par, _key_par, right) :=
  { ([], Map.of_list []) }

| left
; args = separated_list(",", function_parameter)
; right
; { let positional, named = List.partition_map (fun x -> x) args in
    (positional, Map.of_list named)
  }


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
