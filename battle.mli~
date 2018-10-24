open Pokemon
open Moves
open Ptype

(* Our representation of a battle *)
type t


val get_move_from_str: Moves.t list -> string -> Moves.t

(*[can_use_move battle move] is true if the current pokemon can use
the move [move] otherwise it is false*)
val can_use_move: t -> string -> bool



(*[use_move battle move] gives us a new battle after using
[move]*)
val use_move: t -> Moves.t -> unit

val get_player: t -> Pokemon.t

val get_opponent: t -> Pokemon.t


val make_battle: Pokemon.t -> Pokemon.t -> t

