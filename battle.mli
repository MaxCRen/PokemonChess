open Pokemon
open Moves
open Ptype

(* Our representation of a battle *)
type t


(*[can_use_move battle move] is true if the current pokemon can use
the move [move] otherwise it is false*)
val can_mov: t -> Moves.t -> bool

(*[use_move battle move] gives us a new battle after using
[move]*)
val use_move: t -> Moves.t -> t

