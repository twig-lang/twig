/* Modules and signatures. */

%%

/* A module argument. */
%public
let mod_arg :=
  name = "identifier"
; ty = preceded(":", sig_expr)?
; { Ast.ModArgModule { name ; ty }}

| "type" ; ~ = path ; <Ast.ModArgTy>

/* A module (type) expression. */
%public
let sig_expr :=
  joins = separated_nonempty_list("&", path)
; { match joins with
    | p :: [] -> Ast.SigNamed p
    | js      -> Ast.SigJoin js }

/* A module (body) expression. */
%public
let mod_expr :=
  ~ = delimited("(", toplevel*, ")")
; <Ast.ModBody>

| ~ = path
; <Ast.ModPath>
