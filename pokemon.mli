open Ptype
open Moves
(*Our representaion type of a pokemon*)
type t

(*[make_pokemon name poketype moveset attributes status] creates a our representation
of a pokmon type using the given arguments*)
val make_pokemon: string -> (Ptype.t * Ptype.t option) -> 
                            Moves.t list -> float list -> Moves.status option -> t

(** [get_moves pokemon] returns a list of type [Moves.t] that the pokemon
   [pokemon] has available to it *)
val get_moves: t -> Moves.t list

(** [get_name poke] returns the name of pokemon [poke] *)
val get_name: t -> string

(** [get_type poke] returns the [Ptype.t] of pokemon [poke] *)
val get_type: t -> Ptype.t * Ptype.t option

(** [can_raise_stats poke num] returns true if the pokemon [poke] can 
   raise its stats by [num] *)
val can_raise_stats: t -> int -> bool

(** [get_attr poke] returns the stats of pokemon [poke] as a significantly-
    ordered list of floats, where [hp; attack; defense; speed] represent 
    exactly what their names imply *)
val get_attr: t -> float list
(** [get_mult poke] returns the currently applied multipliers to the stats
    of pokemon [poke]. The order of the multipliers in the list corresponds
    to the order in the list returned by [get_attr poke] *)
val get_mult: t -> float list

(** [get_curr_hp poke] returns the current health of pokemon [poke] *)
val get_curr_hp: t -> int

(** [get_max_health] returns the maximum health that pokemon [poke]
    could have *)
val get_max_health: t -> int

(** [get_status poke] returns [Some] of the current status of pokemon [poke] if 
    it has one. Otherwise, returns [None] *)
val get_status: t  ->  Moves.status option

(** [get_confused poke] returns [true] if pokemon [poke] is currently 
    confused *)
val get_confused: t -> bool

(** [get_accuracy poke] returns the current accuracy of pokemon [poke] *)
val get_accuracy: t -> float

(** [change_health poke num] adds [num] to the current health of 
    pokemon [poke] *)
val change_health: t -> int -> unit
