type point = int
type span = point * point
type 'a t = { inner : 'a; location : span }

let create location inner = { inner; location }
let get { inner; _ } = inner
let location { location; _ } = location

let map f t =
  let loc = location t in
  create loc (f @@ get t)
