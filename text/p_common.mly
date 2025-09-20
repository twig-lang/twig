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
; <Ast.Parameter>

| "label"
; ~ = parameter_name
; ~ = preceded(":", ty)
; <Ast.ParameterLabel>

/* Key function parameters. Note the optional default value. */
%public
let key_fn_par :=
  ~ = mode
; ~ = parameter_name
; ~ = preceded("=", expression_nomsg)?
; ~ = preceded(":", ty)
; <Ast.ParameterKey>

| "label"
; ~ = parameter_name
; ty = preceded(":", ty)
; <Ast.ParameterLabel>

/* A path... */
%public
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

/* A parameter-passing mode. */
%public
let mode :=
  is_ref = boption("&")
; is_mut = boption("mut")
; <Ast.Mode>

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
  "const" ; {Ast.PtrConst}
| "mut"   ; {Ast.PtrMut}

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
