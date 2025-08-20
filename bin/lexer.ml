let keywords =
  Parser.
    [
      ("as", As);
      ("break", Break);
      ("const", Const);
      ("continue", Continue);
      ("else", Else);
      ("enum", Enum);
      ("fn", Fn);
      ("if", If);
      ("let", Let);
      ("loop", Loop);
      ("match", Match);
      ("mod", Mod);
      ("mut", Mut);
      ("return", Return);
      ("set", Set);
      ("struct", Struct);
      ("sub", Sub);
      ("type", Type);
      ("union", Union);
      ("unsafe", Unsafe);
      ("where", Where);
      ("while", While);
      ("with", With);
      ("yield", Yield);
    ]
  |> List.to_seq |> Hashtbl.of_seq

let lexer' lexbuf =
  match%sedlex lexbuf with eof -> Parser.Eof | _ -> Parser.Eof

let lexer lexbuf =
  let pre = Sedlexing.lexing_bytes_position_curr lexbuf in
  let tok = lexer' lexbuf in
  let post = Sedlexing.lexing_bytes_position_curr lexbuf in
  (tok, pre, post)
