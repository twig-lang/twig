let keywords =
  let open Token in
  [
    ("break", KBreak);
    ("const", KConst);
    ("continue", KContinue);
    ("else", KElse);
    ("enum", KEnum);
    ("fn", KFn);
    ("if", KIf);
    ("let", KLet);
    ("loop", KLoop);
    ("match", KMatch);
    ("mod", KMod);
    ("mut", KMut);
    ("return", KReturn);
    ("set", KSet);
    ("struct", KStruct);
    ("sub", KSub);
    ("type", KType);
    ("union", KUnion);
    ("unsafe", KUnsafe);
    ("where", KWhere);
    ("while", KWhile);
    ("with", KWith);
    ("yield", KYield);
  ]
  |> List.to_seq |> Hashtbl.of_seq

let lexer' lexbuf =
  match%sedlex lexbuf with eof -> Parser.EOF | _ -> Parser.EOF

let lexer lexbuf =
  let pre = Sedlexing.lexing_bytes_position_curr lexbuf in
  let tok = lexer' lexbuf in
  let post = Sedlexing.lexing_bytes_position_curr lexbuf in
  (tok, pre, post)
