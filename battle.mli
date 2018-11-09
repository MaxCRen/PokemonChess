open Pokemon
open Moves
open Ptype

(**representation of a battle *)
type t

(** [get_move_from_str moves move] returns the move in [moves] corresponding
    to the string [move] *)
val get_move_from_str: Moves.t list -> string -> Moves.t

(**[can_use_move battle move] is true if the current pokemon can use
the move [move] otherwise it is false*)
val can_move: Moves.t -> bool

val deal_damage: int -> Pokemon.t -> unit

(** [use_move battle move bool] the turn of [battle] uses the move [move]
on the opposing pokemon. If [bool] then it is a critical hit and the damage
done is two times.*)
val use_move: t -> Moves.t -> bool -> unit

(** [get_player battle] returns the user's pokemon in the battle [battle] *)
val get_player: t -> Pokemon.t

(** [get_opponent battle] returns the opponent's pokemon in the battle
    [battle] *)
val get_opponent: t -> Pokemon.t


(** [make_battle poke1 poke2] returns a new instance of a battle between
    pokemon [poke1] and pokemon [poke2] *)
val make_battle: Pokemon.t -> Pokemon.t -> t

(** [other_player battle] is the pokemon whose turn it isn't*)
val other_player: t -> Pokemon.t

(**[calc_effective] Calculates the effectiveness of a [move] on pokemon [poke] 
The multiplier is 0, 1/2, 1, 2 as the possible effectivenesses*)
val calc_effective: Moves.t -> Pokemon.t -> float


(**[compare_spped player opponent] is the pokemon [player] or [opponent with the
faster speed attribute] *)
val compare_speed: Pokemon.t -> Pokemon.t -> Pokemon.t

(*[change_turn battle pokemon] changes the turn to [pokemon]*)
val change_turn: t -> Pokemon.t -> unit

(**[get_turn battle] is the pokemon whose turn it is*)
val get_turn: t -> Pokemon.t

