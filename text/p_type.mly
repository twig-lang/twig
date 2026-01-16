/* Types. */

%%

/* Regular types, and an "inference" annotation with _ */
%public
let ty_sink :=
  ~ = ty ; <>
| "_"    ; { Infer.fresh () }

/* Regular types, and types appearing on `type` definitions. */
%public
let ty_all :=
  ~ = ty         ; <>
/*
| ~ = struct_ty  ; <>
| ~ = enum_ty    ; <>
| ~ = union_ty   ; <>
*/

let enum_ty :=
  "enum"
; members = separated_list(",", enum_member)
; { Ty.(Extension (Enumeration members)) }

let enum_member :=
  name      = "identifier"
; arguments = delimited(
  "(",
  separated_list(",", ty),
  ")"
)?
; <>

let struct_ty :=
  "struct"
; members = separated_list(",", struct_member)
; { Ty.(Extension (Structure members)) }

let union_ty :=
  "union"
; members = separated_list(",", struct_member)
; { Ty.(Extension (Union members )) }

let struct_member :=
  ~ = "identifier"
; ~ = preceded(":", ty)
; <>

/* Regular types. */
%public
let ty :=
  ~ = path                   ; <Ty.Named>
| "[" ; "]" ; ~ = ty         ; <Ty.Slice>
| "*" ; ~ = ptr_mut ; ~ = ty ; <Ty.Pointer>
/*
| ~ = lambda_ty              ; <>
*/

| ~ = delimited("[", "integer", "]")
; ~ = ty
; <Ty.Array>

| ts = delimited(
    "(",
    separated_list(",", ty),
    ")"
  )
; {
  match List.length ts with
  | 0 -> Ty.Primitive Ty.Unit
  | 1 -> List.hd ts
  | _ -> Ty.Tuple ts }

/*
let lambda_par :=
  ~ = boption("label")
; ~ = mode
; ~ = ty
; <Ast.AnonParameter>

let lambda_ty :=
  ~ = lambda_fn_kind
; ~ = parameter_list("(", lambda_par, ")")
; ~ = preceded("->", ty)?
; <Ast.TyLambda>

| ~ = lambda_sub_kind
; ~ = parameter_list("[", lambda_par, "]")
; ~ = preceded("->", ty)?
; <Ast.TyLambda>
*/
