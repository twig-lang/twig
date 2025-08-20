let () =
  let input = "fn unit : () = ();" in
  let _ast = Option.get @@ Parse.parse_string input in
  ()
