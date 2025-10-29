open Cmdliner

let process_file path (fails : bool) =
  let input = In_channel.open_text path in
  let passed =
    try
      let definitions = Text.Parse.parse_chan ~fname:path input in

      if not fails then Printexc.record_backtrace true;

      let m = Frontend.Infer.tree_of_toplevels definitions in
      let _resolved = Frontend.Infer.f m in
      not @@ fails
    with
    | Frontend.Infer.TypeMismatch (l, r) ->
        let fmt_tv tv =
          let tv = Frontend.Infer.get tv in
          match tv with Some _ -> "<resolved>" | None -> "<unresolved>"
        in
        Frontend.Ty.(
          Printf.eprintf "type mismatch: %a != %a\n" (fmt ~tv:fmt_tv) l
            (fmt ~tv:fmt_tv) r);
        Printexc.print_backtrace Out_channel.stderr;
        fails
    | Frontend.Mode.ProjectionFailure (target, source) ->
        Format.eprintf "projection failure: %a <- %a\n" Frontend.Mode.pp target
          Frontend.Mode.pp source;
        Printexc.print_backtrace Out_channel.stderr;
        fails
    | Failure message ->
        Printf.eprintf "failwith: %s\n" message;
        Printexc.print_backtrace Out_channel.stderr;
        fails
  in

  if not @@ passed then failwith "twig check : failed"

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
