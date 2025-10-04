open Cmdliner

let process_file path (fails : bool) =
  let input = In_channel.open_text path in
  let raw_ast = Option.get @@ Text.Parse.parse_chan ~fname:path input in

  let passed =
    try
      Printexc.record_backtrace true;
      let _ = Frontend.Of_ast.of_ast raw_ast in
      not @@ fails
    with
    | Frontend.Of_ast.TypeMismatch (l, r) ->
        Frontend.Tree.(
          Printf.eprintf "type mismatch: %a != %a\n" fmt_ty l fmt_ty r);
        fails
    | Failure message ->
        Printf.eprintf "failwith: %s\n" message;
        Printexc.print_backtrace Out_channel.stderr;
        fails
  in

  if not @@ passed then failwith "check failed"

let check_input =
  let doc = "File to process" in
  Arg.(value & pos 0 string "default?" & info [] ~doc ~docv:"PATH")

let should_fail =
  let doc = "This check should fail" in
  Arg.(value & flag & info [ "failing" ] ~doc)

let check_cmd =
  Cmd.v (Cmd.info "check")
  @@ Term.(const process_file $ check_input $ should_fail)

let cmd =
  let info = Cmd.info "twig" in
  Cmd.group info [ check_cmd ]

let () = exit (Cmd.eval cmd)
