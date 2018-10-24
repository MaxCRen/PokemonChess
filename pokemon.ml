open Moves
open Ptype


type t = {
  name: string;
  pokeType: (Ptype.t * Ptype.t option);
  moveSet: Moves.t list;
  (*contains the multiplier for attributes, it is initializes as all 1's*)
  attr_mult: int list;
  (* attributes [hp; attack; defense; speed] *)
  attributes: int list;
  status : Moves.status option;

  confused: bool;
  accuracy: float
}

let make_pokemon n typ mset attr stat = {
  name = n;
  pokeType = typ;
  attr_mult = [1;1;1;1];
  moveSet= mset;
  attributes = attr;
  status = stat;
  confused = false;
  accuracy = 1.
}