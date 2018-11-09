open Ptype
open Moves
(*Our representaion type of a pokemon*)
type t

(*[make_pokemon name poketype moveset attributes status] creates a our representation
  of a pokmon type using the given arguments*)
val make_pokemon: string -> (Ptype.t * Ptype.t option) -> 
  Moves.t list -> float list -> t

(** [get_moves poke] returns a list of moves that the pokemon [poke] has
    available to it. *)
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
val get_confused: t -> int

(** [get_accuracy poke] returns the current accuracy of pokemon [poke] *)
val get_accuracy: t -> float

(**[change_health poke health] changes the pokemon's hp by [health] if the new
   hp is greater than the max health, then they gain up to max health, if it is
   less then zero then the pokemon's health becomes 0*)
val change_health: t -> int -> unit

(**[change_attr_mult poke attr] changes the attribute multiplier of pokemon
[poke]*)
val change_attr_mult: t -> float list -> unit

(**[change_status pokemon stats] changes the pokemon [poke]'s status the stats*)
val change_status: t -> Moves.status option -> unit

(**[change_accuracy poke acc] changes the pokemon [poke]'s accuracy to acc*)
val change_accuracy: t -> float -> unit

(**[get_promoted_pawn ()] is the pokemon that represents our promoted pawn*)
val get_promoted_pawn : unit -> t

(**[get_pawn ()] is the pokemon that represents our pawn*)
val get_pawn: unit -> t

(**[get_rook ()] is the pokemon that represents our rook*)
val get_rook: unit -> t

(**[get_bishop ()] is the pokemon that represents our bishop*)
val get_bishop: unit -> t

(**[get_knight ()] is the pokemon that represents our knight*)
val get_knight: unit -> t

(**[get_queen()] is the pokemon that represents our queen *)
val get_queen: unit -> t

(**[get_king ()] is the pokemon that represents our king *)
val get_king: unit -> t

(**[get_struggle ()] is the pokemon move that represents struggle*)
val struggle: unit -> Moves.t

(**[out_of_pp pokemon] is true if all the moves of [pokemon] are out of pp
otherwise it is false*)
val out_of_pp: t -> bool
