open Ptype
open Moves
open Yojson.Basic.Util
(*Our representaion type of a pokemon*)
type t

(*[make_pokemon name poketype moveset attributes status] creates a our representation
of a pokmon type using the given arguments*)
val make_pokemon: string -> (Ptype.t * Ptype.t option) -> 
                            Yojson.Basic.json list -> int list -> Moves.status option -> t
