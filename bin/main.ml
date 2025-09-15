open Cmdliner
open Cmdliner.Term.Syntax

let process_file path =
  let input = In_channel.open_text path in
  let raw_ast = Option.get @@ Parse.parse_chan ~fname:path input in
  let ty_ast = Typed.of_ast raw_ast in
  ignore ty_ast;
  ()

let check_input =
  let doc = "File to process" in
  Arg.(value & pos 0 string "default?" & info [] ~doc ~docv:"PATH")

let check_cmd =
  Cmd.v (Cmd.info "check")
  @@
  let+ check_input = check_input in
  process_file check_input

let cmd =
  let info = Cmd.info "twig" in
  Cmd.group info [ check_cmd ]

let () = exit (Cmd.eval cmd)
