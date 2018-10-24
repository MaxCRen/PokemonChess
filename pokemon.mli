open Ptype
open Moves
(*Our representaion type of a pokemon*)
type t

(*[make_pokemon name poketype moveset attributes status] creates a our representation
of a pokmon type using the given arguments*)
val make_pokemon: string -> (Ptype.t * Ptype.t option) -> 
                            Moves.t list -> float list -> Moves.status option -> t

val get_moves: t -> Moves.t list

val get_name: t -> string

val get_type: t -> Ptype.t * Ptype.t option

val can_raise_stats: t -> int -> bool

val get_attr: t -> float list

val get_mult: t -> float list

val get_curr_hp: t -> int

val get_status: t  ->  Moves.status option

val get_confused: t -> bool

val get_accuracy: t -> float

val change_health: t -> int -> unit
