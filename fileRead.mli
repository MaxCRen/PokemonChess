open Pokemon
open Ptype
open Moves

(*[get_poke index] will give the pokemon.t representation of the 
pokemon at line [index]*)
val get_poke: int -> string -> Pokemon.t

(*[get_type type] will give the ptype.t representation of the pokemon
type named [type]*)
val get_type: string -> string -> Ptype.t