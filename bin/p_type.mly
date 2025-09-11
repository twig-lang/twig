/* Types. */

%%

/* Regular types, and types appearing on `type` definitions. */
%public
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

/* Regular types. */
%public
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

