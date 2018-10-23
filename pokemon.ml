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