open Moves
open Ptype


type t = {
  name: string;
  pokeType: (Ptype.t * Ptype.t option);
  moveSet: Moves.t list;
  (* attributes [hp; attack; defense; spatk; spdef; speed;] *)
  attributes: int list;
  status : Moves.status option
}

let make_pokemon n typ mset attr stat = {
  name = n;
  pokeType = typ;
  moveSet= mset;
  attributes = attr;
  status = stat;
}