open Pokemon
open Moves
open Ptype

(**representation of a battle *)
type t

exception Charging of int

(** [get_move_from_str moves move] returns the move in [moves] corresponding
    to the string [move] *)
val get_move_from_str: Moves.t list -> string -> Moves.t

(**[can_use_move battle move] is true if the current pokemon can use
the move [move] otherwise it is false*)
val can_move: Moves.t -> bool

val deal_damage: int -> Pokemon.t -> unit

(**[use_move battle move] gives us a new battle after using
[move]*)
val use_move: t -> Moves.t -> bool -> unit

(** [get_player battle] returns the user's pokemon in the battle [battle] *)
val get_player: t -> Pokemon.t

(** [get_opponent battle] returns the opponent's pokemon in the battle
    [battle] *)
val get_opponent: t -> Pokemon.t


(** [make_battle poke1 poke2] returns a new instance of a battle between
    pokemon [poke1] and pokemon [poke2] *)
val make_battle: Pokemon.t -> Pokemon.t -> t

val other_player: t -> Pokemon.t

val calc_effective: Moves.t -> Pokemon.t -> float


(**[compare_spped player opponent] is the pokemon [player] or [opponent with the
faster speed attribute] *)
val compare_speed: Pokemon.t -> Pokemon.t -> Pokemon.t

val change_turn: t -> Pokemon.t -> unit

val get_turn: t -> Pokemon.t

