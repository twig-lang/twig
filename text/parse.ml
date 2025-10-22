module I = Parser.MenhirInterpreter
module E = MenhirLib.ErrorReports

exception Error of string

let rec loop lexer lexbuf (checkpoint : 'a I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = lexer () in
      let checkpoint = I.offer checkpoint token in
      loop lexer lexbuf checkpoint
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop lexer lexbuf checkpoint
  | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      loop lexer lexbuf checkpoint
  | I.HandlingError env ->
      let state = I.current_state_number env in
      let message = Messages.message state in
      Printf.eprintf "on state %d\n" state;
      raise (Error message)
  | I.Accepted x -> x
  | I.Rejected -> failwith "input rejected"

let parse lexbuf =
  let fmt_pos lb =
    let pos = Sedlexing.lexing_bytes_position_curr lb in
    Printf.sprintf "[%s %d:%d]" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in
  let lexer () = Lexer.lexer lexbuf in

  try
    loop lexer lexbuf
      (Parser.Incremental.main (fst @@ Sedlexing.lexing_positions lexbuf))
  with Parser.Error ->
    let message = fmt_pos lexbuf ^ " syntax error" in
    failwith message

let parse_chan ?(fname = "*channel*") chan =
  let lb = Sedlexing.Utf8.from_channel chan in
  Sedlexing.set_filename lb fname;
  parse lb

let parse_string ?(fname = "*string*") str =
  let lb = Sedlexing.Utf8.from_string str in
  Sedlexing.set_filename lb fname;
  parse lb
