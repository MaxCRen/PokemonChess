open Moves
open Ptype



type t = {
  name: string;
  pokeType: (Ptype.t option * Ptype.t option);
  moveSet: Moves.t list;
  (* attributes [hp; attack; defense; speed; special] *)
  attributes: int list;
  status : Moves.status
}