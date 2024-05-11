type t = private string

val v : string -> t
val vf : ('a, unit, t, t) format4 -> 'a
val join : t -> t -> t
val ( / ) : t -> t -> t
val add_ext : string -> t -> t
val rem_ext : t -> t
val mod_ext : string -> t -> t
val cwd : unit -> t
val relative : ?to_:t -> t -> t
val resolve : t -> t
val dirname : t -> t
val is_abs : t -> bool
val is_rel : t -> bool

module Set : Set.S with type elt = t
