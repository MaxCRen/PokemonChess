open Pokemon

(*[get_poke index file_name] will give the pokemon.t representation of the 
pokemon at line [index] from file [file_name]*)
val get_poke: int -> string -> Pokemon.t

(*[get_type index file_name] will give the ptype.t representation of the pokemon
type at line [index] from file [file_name]*)
val get_type: int -> string -> Ptype.t