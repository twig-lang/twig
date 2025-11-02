open Frontend
module I = Parser.MenhirInterpreter
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* file name, line, column * message *)
exception Error of string * int * int * string

type state = {
  input : string;
  lexer : unit -> Parser.token * Lexing.position * Lexing.position;
  buffer : (Lexing.position * Lexing.position) E.buffer;
}

let show input position =
  E.(extract input position |> sanitize |> compress |> shorten 30)

let get text env i =
  let (I.Element (_, _, l, r)) = Option.get @@ I.get i env in
  show text (l, r)

let rec loop st lexbuf (checkpoint : 'a I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = st.lexer () in
      let checkpoint = I.offer checkpoint token in
      loop st lexbuf checkpoint
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop st lexbuf checkpoint
  | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      loop st lexbuf checkpoint
  | I.HandlingError env ->
      let _location = L.range (E.last st.buffer) in
      let _message = Messages.message (I.current_state_number env) in
      let indication = E.show (show st.input) st.buffer in
      Printf.eprintf "MESSAGE: %s\n" indication;

      let state = I.current_state_number env in
      let message = Messages.message state in
      let position = Sedlexing.lexing_bytes_position_curr lexbuf in
      let file = position.pos_fname in
      let line = position.pos_lnum in
      let column = position.pos_cnum - position.pos_bol in
      Printf.eprintf "on state %d\n" state;
      raise (Error (file, line, column, message))
  | I.Accepted x -> x
  | I.Rejected -> failwith "input rejected"

let parse input lexbuf =
  let fmt_pos lb =
    let pos = Sedlexing.lexing_bytes_position_curr lb in
    Printf.sprintf "[%s %d:%d]" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in
  let lexer () = Lexer.lexer lexbuf in

  let st =
    let buffer, lexer = E.wrap_supplier lexer in
    { lexer; buffer; input }
  in

  try
    loop st lexbuf
      (Parser.Incremental.main (fst @@ Sedlexing.lexing_positions lexbuf))
  with Parser.Error ->
    let message = fmt_pos lexbuf ^ " syntax error" in
    failwith message

let parse_string ?(fname = "*string*") str =
  ignore @@ Files.add' fname str;
  let lb = Sedlexing.Utf8.from_string str in
  Sedlexing.set_filename lb fname;
  parse str lb

let parse_file path =
  let data = Files.add path in
  let lb = Sedlexing.Utf8.from_string data in
  Sedlexing.set_filename lb path;
  parse data lb
