/* Toplevel statements. */

%%

%public
let toplevel :=
  ~ = terminated(top_all, ";") ; <>

%public
let top_all :=
  ~ = fn_definition    ; <>
| ~ = sub_definition   ; <>
| ~ = const_definition ; <>
| ~ = type_definition  ; <>
| ~ = extern           ; <>
| ~ = top_with         ; <>
| ~ = mod_definition   ; <>

let fn_definition :=
  unsafep = boption("unsafe")
; "fn"
; name = fn_name
; parameters = parameter_list2("(", fn_par, key_fn_par, ")")
; ty = preceded("->", ty)?
; value = preceded("=", expr_all)?
; { let (pos_parameters, key_parameters) = parameters in
    Ast.TopFnDefinition {
      unsafep ;
      name ;
      pos_parameters ;
      key_parameters ;
      ty ;
      value } }

let sub_definition :=
  unsafep = boption("unsafe")
; "sub"
; mode = mode
; ~ = yields
; name = fn_name
; parameters = parameter_list2("[", fn_par, key_fn_par, "]")
; ty = preceded("->", ty)?
; value = preceded("=", expr_all)?
; { let (pos_parameters, key_parameters) = parameters in
    Ast.TopSubDefinition {
      unsafep ;
      yields ;
      mode ;
      name ;
      pos_parameters ;
      key_parameters ;
      ty ;
      value } }

let const_definition :=
  "const"
; name = "identifier"
; ty = preceded(":", ty)
; value = preceded("=", expression)
; { Ast.TopConstDefinition { name ; ty ; value } }

let type_definition :=
  "type"
; name = "identifier"
; "="
; ty = ty_all
; { Ast.TopTypeDefinition { name ; ty } }

| "type"
; ~ = "identifier"
; <Ast.TopTypeAbstract>

let extern :=
  "extern"
; abi = "string"?
; "fn"
; name = "identifier"
; parameters = loption(parameter_list("(", fn_par, ")"))
; ty = preceded("->", ty)?
; { Ast.TopExtern { abi ; name ; parameters ; ty } }

let top_with :=
  "with"
; imports = boption("import")
; path = import_path
; { Ast.TopWith { imports ; path } }

let mod_definition :=
  "mod"
; name = "identifier"
; args = preceded("!", parameter_list("(", mod_par, ")"))?
; signature = preceded(":", sig_expr)?
; "="
; value = mod_expr
; { let args = Option.value ~default:[] args in
    Ast.TopModDefinition { name ; args ; signature ; value }}

| "mod" ; "type"
; name = "identifier"
; args = preceded("!", parameter_list("(", mod_par, ")"))?
; "="
; value = mod_expr
; { let args = Option.value ~default:[] args in
    Ast.TopSigDefinition { name ; args ; value }}

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

let fn_name :=
  ~ = "identifier"
; <Ast.FnNamed>

| ~ = delimited("(", operator, ")")
; <Ast.FnOperator>
