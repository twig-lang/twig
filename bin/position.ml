type t = { byte_from : int; byte_to : int; line : int; column : int }

let create ?offset:(off = 0) () =
  { byte_from = off; byte_to = off; line = 0; column = 0 }

let set_from p f = { p with byte_from = f }
let set_to p t = { p with byte_to = t }

let step p c =
  let p =
    if c == Uchar.of_char '\n' then { p with line = p.line + 1; column = 0 }
    else { p with column = p.column + 1 }
  in
  { p with byte_to = p.byte_to + Uchar.utf_8_byte_length c }
