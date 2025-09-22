module type STAGE = sig
  (* This stage's type variable. *)
  type ty_variable
end

module Tree (S : STAGE) = struct
  type t = |
end

module type CVT = sig
  module From : STAGE
  module To : STAGE

  val cvt : Tree(From).t -> Tree(To).t
end
