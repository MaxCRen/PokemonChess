open Pokemon
open Ptype
open Moves

(*[get_poke index] will give the pokemon.t representation of the 
pokemon at line [index]*)
val get_poke: int ->  Pokemon.t

(*[get_move move] will give the Moves.t representation of the 
move [move]*)
val get_move: string ->  Moves.t

(*[get_type type] will give the ptype.t representation of the pokemon
type named [type]*)
val get_type: string -> Ptype.t