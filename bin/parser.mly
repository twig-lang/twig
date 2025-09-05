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
%token Question "?"
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
%token Where "where"
%token Then "then"
%token Do "do"
%token Extern "extern"
%token Import "import"
%token When "when"
%token True "true"
%token False "false"

%start main
%type<Ast.toplevel list> main

%type<Ast.expr> block

%%

let main :=
  ~ = file ; Eof ; <>

let file :=
  ~ = toplevel* ; <>

let toplevel :=
  ~ = top_all
  ; ";"
  ; <>

let top_all :=
  ~ = function_definition ; <>
| ~ = constant_definition ; <>
| ~ = type_definition     ; <>
| ~ = extern              ; <>
| ~ = top_with            ; <>
| ~ = sub_definition      ; <>
| ~ = mod_definition      ; <>

let mod_definition :=
  "mod"
  ; name = "identifier"
  ; args = preceded("!", parameter_list(mod_arg))?
  ; signature = preceded(":", sig_expr)?
  ; "="
  ; value = mod_expr
  ; { let args = Option.value ~default:[] args in
      Ast.ModDefinition { name ; args ; signature ; value }}

| "mod" ; "type"
  ; name = "identifier"
  ; args = preceded("!", parameter_list(mod_arg))?
  ; "="
  ; value = mod_expr
  ; { let args = Option.value ~default:[] args in
      Ast.SigDefinition { name ; args ; value }}

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
  "("
  ; body = toplevel*
  ; ")"
  ; <Ast.ModBody>
| ~ = path ; <Ast.ModPath>

let top_with :=
  "with"
  ; imports = boption("import")
  ; path = import_path
  ; { Ast.ToplevelWith { imports ; path } }

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

let extern :=
  "extern"
  ; abi = "string"?
  ; "fn"
  ; name = "identifier"
  ; parameters = parameter_list(fn_par)
  ; ty = preceded(":", ty)?
  ; { Ast.Extern { abi ; name ; parameters ; ty } }

let type_definition :=
  "type"
  ; name = "identifier"
  ; "="
  ; ty = ty_all
  ; { Ast.TypeDefinition { name ; ty } }
| "type"
  ; name = "identifier"
  ; <Ast.TypeAbstract>

let ty_all :=
  ~ = ty        ; <>
| ~ = struct_ty ; <>
| ~ = enum_ty   ; <>
| ~ = union_ty   ; <>

let enum_ty :=
  "enum"
  ; members = separated_list(",", enum_member)
  ; { Ast.EnumTy { members } }

let enum_member :=
  name = "identifier"
  ; args = delimited(
    "(",
    separated_list(",", ty),
    ")"
  )?
  ; {
  match args with
  | None -> Ast.EnumMember { name }
  | Some args -> Ast.EnumMemberArgs { name ; args }
  }

let struct_ty :=
  "struct"
  ; members = separated_list(",", struct_member)
  ; { Ast.StructTy { members } }

let union_ty :=
  "union"
  ; members = separated_list(",", struct_member)
  ; { Ast.UnionTy { members } }

let struct_member :=
  name = "identifier"
  ; ":"
  ; ty = ty
  ; { Ast.StructMember { name ; ty } }

let constant_definition :=
  "const"
  ; name = "identifier"
  ; ty = preceded(":", ty)
  ; value = preceded("=", expression)
  ; { Ast.ConstantDefinition { name ; ty ; value } }

let yields :=
  "if"    ; {Ast.YieldIf}
| "while" ; {Ast.YieldWhile}
|           {Ast.Returns}

let function_definition :=
  unsafep = boption("unsafe")
  ; "fn"
  ; ~ = yields
  ; name = "identifier"
  ; parameters = parameter_list(fn_par)
  ; ty = preceded(":", ty)?
  ; value = preceded("=", expr_all)?
  ; { Ast.FunctionDefinition { unsafep ; yields ; name ; parameters ; ty ; value } }

let sub_definition :=
  unsafep = boption("unsafe")
  ; "sub"
  ; mode = mode
  ; ~ = yields
  ; name = "identifier"
  ; parameters = parameter_list(fn_par)
  ; ty = preceded(":", ty)?
  ; value = preceded("=", expr_all)?
  ; { Ast.SubDefinition { unsafep ; yields ; mode ; name ; parameters ; ty ; value } }

let parameter_list(par) :=
  pars = delimited("|", separated_list(",", par), "|")?
  ; { Option.value ~default:[] pars }

let mode :=
  is_ref = boption("&")
  ; is_mut = boption("mut")
  ; { Ast.Mode { is_ref ; is_mut} }

let fn_par :=
  ~ = mode
  ; name = "identifier"
  ; key = "identifier"?
  ; default = preceded("=", expression_nomsg)?
  ; ty = preceded(":", ty)
  ; { FnParameter { mode; name ; key ; ty ; default } }

let path :=
  path = separated_nonempty_list(".", path_atom) ; <Ast.Member>

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
  ; {
   match args with
   | None -> Ast.Atom name
   | Some args -> Ast.Call { name ; args }
  }

let ty :=
  ts = delimited(
    "(",
    separated_list(",", ty),
    ")"
  )
  ; {
   match List.length ts with
   | 0 -> Ast.UnitTy
   | 1 -> List.hd ts
   | _ -> Ast.Tuple ts
  }
| ~ = path           ; <Ast.Named>
| ~ = delimited("[", "integer", "]")
  ; ~ = ty
  ; <Ast.Array>
| "[" ; "]" ; ~ = ty ; <Ast.Slice>
| "*" ; ~ = ptr_mut ; ~ = ty ; <Ast.Pointer>

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

| ~ = preceded("return", expression) ; <Ast.Return>

| ~ = top_all    ; <Ast.Top>

let match_exp :=
  "match"
  ; scrutinee = expression_nw
  ; "with"
  ; cases = separated_list(",", match_case)
  ; { Ast.Match { scrutinee ; cases } }

let match_case :=
  pat = pattern
  ; "="
  ; body = expression
  ; { Ast.Case { pat ; body} }

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
  ; <Ast.Yield>

let operator :=
  ~ = "operator" ; <>
| "*"            ; {"*"}
| "|"            ; {"|"}

let set_exp :=
  "set"
  ; lval = expression
  ; "="
  ; rval = expression
  ; { Ast.Set { lval ; rval } }

let if_exp :=
  "if"
  ; condition = expression
  ; "then"
  ; taken = expr_all
  ; "else"
  ; not_taken = expr_all
  ; { Ast.If { condition ; taken ; not_taken } }
| "if"; "let"
  ; bind = pattern
  ; ty = preceded(":", ty)?
  ; value = preceded("=", expression)
  ; "then"
  ; taken = expr_all
  ; "else"
  ; not_taken = expr_all
  ; { Ast.IfLet { bind; ty; value ; taken ; not_taken } }

let while_exp :=
  "while"
  ; condition = expression
  ; "do"
  ; body = expr_all
  ; { Ast.While { condition ; body } }
| "while" ; "let"
  ; bind = pattern
  ; ty = preceded(":", ty)?
  ; "="
  ; value = expression
  ; "do"
  ; body = expr_all
  ; { Ast.WhileLet { bind ; ty ; value ; body } }

let when_exp :=
  "when"
  ; condition = expression
  ; "do"
  ; taken = expr_all
  ; { Ast.When { condition ; taken } }
| "when" ; "let"
  ; bind = pattern
  ; ty = preceded(":", ty)?
  ; "="
  ; value = expression
  ; "do"
  ; body = expr_all
  ; { Ast.WhenLet { bind ; ty ; value ; body } }

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
          |> List.map (fun (Ast.FnArgument a) -> (Option.get a.key, a.mode, a.value))
        in Ast.Update (left, args) }

let expression_nw :=
  "unsafe" ; ~ = expression_nw ; <Ast.Unsafe>
| ~ = delimited(
    "[",
    separated_list(",", expression),
    "]"
  )
  ; <Ast.ArrayLit>
| ~ = msg_exp ; <>
| "*"
  ; ~ = ptr_mut
  ; ~ = expression_nw
  ; <Ast.Addressof>
| "@"
  ; ~ = mode
  ; ~ = expression_nw
  ; <Ast.Deref>

let msg_exp :=
  recv = expression_nomsg
  ; msgs = message*
  ; { List.fold_left (fun r m -> Ast.Send { recv = r ; msg = m } ) recv msgs }

let expression_nomsg :=
  ~ = primary ; <>
| callee = expression_nomsg
  ; args = delimited("(", arglist ,")")
  ; { Ast.FnCall { callee ; args } }
| callee = expression_nomsg
  ; args = delimited("[", arglist ,"]")
  ; { Ast.FnCall { callee ; args } }
| ~ = expression_nomsg
  ; "as"
  ; ~ = ty
  ; <Ast.Cast>

let arglist :=
  ~ = separated_list(",", fn_arg) ; <>
| positional = separated_list(",", fn_arg)
  ; ";"
  ; keys = separated_list(",", key_fn_arg)
  ; { List.append positional keys }

let primary :=
  ~ = path      ; <Ast.Variable>
| ~ = block     ; <>
| ~ = "integer" ; <Ast.Integer>
| s = "string"+ ; {Ast.String (String.concat "" s)}
| ~ = "char"    ; <Ast.Char>
| ~ = "real"    ; <Ast.Real>
| "true"        ; {Ast.Bool true}
| "false"       ; {Ast.Bool false}

let let_exp :=
  "let"
  ; bind = pattern
  ; ty = preceded(":", ty)?
  ; "="
  ; ~ = mode
  ; value = expression
  ; { Ast.Let { bind ; ty ; mode ; value } }

let block :=
  "("
  ; items = separated_list(";", expr_all)
  ; ")"
  ; { match List.length items with
      | 0 -> Ast.Unit
      | 1 -> List.hd items
      | _ -> Ast.Block items }

let message :=
  ~ = call_message ; <>
| ~ = op_message ; <>

let mode_exp :=
  mode = mode
  ; value = expression_nomsg
  ; <>

let op_message :=
  name = operator
  ; arg = mode_exp
  ; { Ast.OpMessage { name ; arg } }

let call_message :=
  name = path
  ; args = delimited("(", arglist ,")")
  ; tail = preceded(":", mode_exp)?
  ; {Ast.FnMessage { name ; args ; tail }}
| name = path
  ; args = delimited("[", arglist ,"]")
  ; tail = preceded(":", mode_exp)?
  ; {Ast.SubMessage { name ; args ; tail }}
| name = path
  ; tail = preceded(":", mode_exp)?
  ; {Ast.MemberMessage { name ; tail }}

let fn_arg :=
  mode = mode
  ; value = expression
  ; { Ast.FnArgument { key = None; mode ; value } }

let key_fn_arg :=
  key = terminated("identifier", "=")
  ; mode = mode
  ; value = expression
  ; { Ast.FnArgument { key = Some key ; mode ; value } }
| name = "identifier"
  ; mode = mode
  ; { Ast.FnArgument {
        key = Some name ;
        mode ;
        value = Ast.(Variable (Atom name)) } }
