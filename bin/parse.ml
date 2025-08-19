let parse' lb =
  let lexer () = Lexer.lexer lb in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.main in

  parser lexer

let parse_chan chan =
  let lb = Sedlexing.Utf8.from_channel chan in
  parse' lb

let parse_string str =
  let lb = Sedlexing.Utf8.from_string str in
  parse' lb
