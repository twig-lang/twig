let ppos o lb =
  let pos = Sedlexing.lexing_bytes_position_curr lb in
  Printf.fprintf o "[%s %d:%d]" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse' lb =
  let lexer () = Lexer.lexer lb in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.main in

  try Some (parser lexer)
  with Parser.Error ->
    Printf.printf "%a syntax error\n" ppos lb;
    None

let parse_chan ?(fname = "*channel*") chan =
  let lb = Sedlexing.Utf8.from_channel chan in
  Sedlexing.set_filename lb fname;
  parse' lb

let parse_string ?(fname = "*string*") str =
  let lb = Sedlexing.Utf8.from_string str in
  Sedlexing.set_filename lb fname;
  parse' lb
