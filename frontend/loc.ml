type point = int
type span = point * point
type 'a t = { inner : 'a; location : span }

let create inner location = { inner; location }
let get { inner; _ } = inner
let location { location; _ } = location
