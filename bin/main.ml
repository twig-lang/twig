let () =
  let path = "test.tw" in
  let input = In_channel.open_text path in
  let _ast = Option.get @@ Parse.parse_chan ~fname:path input in
  ()
