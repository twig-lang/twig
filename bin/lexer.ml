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
      ("then", Then);
      ("do", Do);
      ("extern", Extern);
      ("import", Import);
      ("when", When);
      ("true", True);
      ("false", False);
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
      ("!", Bang);
    ]
  |> List.to_seq |> Hashtbl.of_seq

let id2kw id =
  Option.value ~default:(Parser.Identifier id) @@ Hashtbl.find_opt keywords id

let op2kw op =
  Option.value ~default:(Parser.Operator op) @@ Hashtbl.find_opt operators op

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let id_head = [%sedlex.regexp? xid_start | '_']
let id_tail = [%sedlex.regexp? xid_continue | '_']

let lex_str_escape lexbuf =
  match%sedlex lexbuf with
  | 'n' -> Uchar.of_char '\n'
  | 'r' -> Uchar.of_char '\r'
  | 't' -> Uchar.of_char '\t'
  | '\\' -> Uchar.of_char '\\'
  | '"' -> Uchar.of_char '"'
  | '\'' -> Uchar.of_char '\''
  | '0' -> Uchar.of_int 0
  | _ ->
      ignore @@ Sedlexing.next lexbuf;
      failwith @@ "unknown character escape: \\" ^ Sedlexing.Utf8.lexeme lexbuf

let rec lex_str buf lexbuf =
  match%sedlex lexbuf with
  | Plus (Compl ('"' | '\\')) ->
      Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
      lex_str buf lexbuf
  | '"' -> Parser.String (Buffer.contents buf)
  | '\\' ->
      let escape = lex_str_escape lexbuf in
      Buffer.add_utf_8_uchar buf escape;
      lex_str buf lexbuf
  | _ ->
      ignore @@ Sedlexing.next lexbuf;
      failwith @@ "unknown string character `"
      ^ Sedlexing.Utf8.lexeme lexbuf
      ^ "`"

let rec lex_comment nest lexbuf =
  match%sedlex lexbuf with
  | '{' -> lex_comment (succ nest) lexbuf
  | '}' -> if nest = 1 then lexer' lexbuf else lex_comment (pred nest) lexbuf
  | Plus (Compl ('{' | '}')) -> lex_comment nest lexbuf
  | eof ->
      ignore @@ Sedlexing.next lexbuf;
      failwith @@ "unexpected eof in comment"
  | _ ->
      ignore @@ Sedlexing.next lexbuf;
      failwith @@ "unknown char in comment:`"
      ^ Sedlexing.Utf8.lexeme lexbuf
      ^ "`"

and lexer' lexbuf =
  match%sedlex lexbuf with
  | Plus white_space -> lexer' lexbuf
  | id_head, Star id_tail -> id2kw (Sedlexing.Utf8.lexeme lexbuf)
  | '(' -> Parser.LParen
  | ')' -> Parser.RParen
  | '[' -> Parser.LBrac
  | ']' -> Parser.RBrac
  | Plus (Chars "+*/-,.;:!@#$%&=?!<>^" | math | other_math) ->
      op2kw (Sedlexing.Utf8.lexeme lexbuf)
  | number -> Parser.Integer (int_of_string @@ Sedlexing.Utf8.lexeme lexbuf)
  | '"' -> lex_str (Buffer.create 16) lexbuf
  | '{' -> lex_comment 1 lexbuf
  | eof -> Parser.Eof
  | _ ->
      ignore @@ Sedlexing.next lexbuf;
      failwith @@ "unknown character `" ^ Sedlexing.Utf8.lexeme lexbuf ^ "`"

let lexer lexbuf =
  let pre = Sedlexing.lexing_bytes_position_curr lexbuf in
  let tok = lexer' lexbuf in
  let post = Sedlexing.lexing_bytes_position_curr lexbuf in
  (tok, pre, post)
