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

let operators =
  Parser.
    [
      ("=", Equal);
      ("*", Star);
      ("&", Amp);
      ("@", At);
      (".", Dot);
      (":", Colon);
      (",", Comma);
      (";", Semicolon);
      ("|", Bar);
    ]
  |> List.to_seq |> Hashtbl.of_seq

let id2kw id =
  Option.value ~default:(Parser.Identifier id) @@ Hashtbl.find_opt keywords id

let op2kw op =
  Option.value ~default:(Parser.Operator op) @@ Hashtbl.find_opt operators op

let rec lexer' lexbuf =
  match%sedlex lexbuf with
  | Plus white_space -> lexer' lexbuf
  | xid_start, Star xid_continue -> id2kw (Sedlexing.Utf8.lexeme lexbuf)
  | '(' -> Parser.LParen
  | ')' -> Parser.RParen
  | Plus (Chars "+*/-,.;:!@#$%&=?!<>^" | math | other_math) ->
      op2kw (Sedlexing.Utf8.lexeme lexbuf)
  | eof -> Parser.Eof
  | _ -> failwith "unknown character"

let lexer lexbuf =
  let pre = Sedlexing.lexing_bytes_position_curr lexbuf in
  let tok = lexer' lexbuf in
  let post = Sedlexing.lexing_bytes_position_curr lexbuf in
  (tok, pre, post)
