(* A "port" of recursion schemes *)

open Arrow
open Combinator

module Scheme (F : Functor.S) = struct
  type 'a t = Fix of 'a t F.t [@@unboxed]
  type 'a free = Pure of 'a | Impure of 'a free F.t
  type 'a cofree = Cofree of 'a * 'a cofree F.t
  type 'a algebra = 'a F.t -> 'a
  type 'a coalgebra = 'a -> 'a F.t
  type 'a r_algebra = ('a t * 'a) F.t -> 'a
  type 'a r_coalgebra = 'a -> ('a t, 'a) Either.t F.t
  type 'a cv_algebra = 'a cofree F.t -> 'a
  type 'a cv_coalgebra = 'a -> 'a free F.t

  let in_ x = Fix x
  let out_ (Fix x) = x
  let rec bottom_up f = out_ >>> F.map (bottom_up f) >>> in_ >>> f
  let rec top_down f = in_ <<< F.map (top_down f) <<< out_ <<< f

  module Folds = struct
    let rec cata (f : 'a algebra) = out_ >>> F.map (cata f) >>> f
    let rec para (f : 'a r_algebra) = out_ >>> F.map (id &&& para f) >>> f

    let histo (f : 'a cv_algebra) =
      let cosnd (Cofree (_, x)) = x in
      let conew (a, b) = Cofree (a, b) in

      let rec work x = (out_ >>> F.map work >>> (f &&& id) >>> conew) x in

      work >>> cosnd
  end

  module Unfolds = struct
    let rec ana (f : 'a coalgebra) = in_ <<< F.map (ana f) <<< f
    let rec apo (f : 'a r_coalgebra) = in_ <<< F.map (id ||| apo f) <<< f

    let rec futu (f : 'a cv_coalgebra) =
      let rec work = function
        | Pure x -> futu f x
        | Impure y -> in_ (F.map work y)
      in
      in_ <<< F.map work <<< f
  end

  module Refolds = struct
    open Folds
    open Unfolds

    let rec hylo (a : 'a algebra) (co : 'a coalgebra) =
      co >>> F.map (hylo a co) >>> a

    let hypo (a : 'a r_algebra) (co : 'a r_coalgebra) = apo co >>> para a
    let chrono (a : 'a cv_algebra) (co : 'a cv_coalgebra) = futu co >>> histo a
    let rec elgot (a : 'a algebra) co = co >>> (id ||| F.map (elgot a co) >>> a)

    let rec coelgot a (co : 'a coalgebra) =
      a <<< (id &&& (F.map (coelgot a co) <<< co))
  end
end
