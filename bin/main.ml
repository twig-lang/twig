open Cmdliner
open Cmdliner.Term.Syntax

let process_file path =
  let input = In_channel.open_text path in
  let raw_ast = Option.get @@ Text.Parse.parse_chan ~fname:path input in
  ignore
    (try Frontend.Of_ast.of_ast raw_ast
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       Printexc.raise_with_backtrace e bt)

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
