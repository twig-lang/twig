type mutability = Immutable | Mutable
type sharing = Data | Reference
type projection = Value | Projection
type t

exception ProjectionFailure of t * t

val create :
  ?project:projection -> ?mut:mutability -> ?share:sharing -> unit -> t

val is_mutable : t -> bool
val is_reference : t -> bool
val is_projection : t -> bool
val ( <: ) : t -> t -> bool
val ( >: ) : t -> t -> bool
val try_project : t -> t -> t option
val project : t -> t -> t
val unproject : t -> t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val fmt_mutability : out_channel -> mutability -> unit
val fmt_sharing : out_channel -> sharing -> unit
val fmt_projection : out_channel -> projection -> unit
val fmt : out_channel -> t -> unit
