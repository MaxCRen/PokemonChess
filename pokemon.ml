open Moves
open Ptype
open Yojson.Basic.Util


type t = {
  name: string;
  pokeType: (Ptype.t * Ptype.t option);
  moveSet: Yojson.Basic.json list;
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

let from_json json =
  let name = json |> member "name" |> to_string in
  let ptype = json |> member "ptype" |> to_list |> List.map (to_string) in 
  let moves = json |> member "moves" |> to_list in
  let attr = json |> member "stats" |> to_list |> List.map (to_int) in  
  (* there are max of 2 types, and we parse from a list of types ...
   * either there's one or 2, so we match on these *)
  match ptype with 
  | [] -> failwith "Error"
  | h::[] -> 
    make_pokemon name 
      (Ptype.makeType h [], None) moves attr None
  | h::t::[] ->  
    make_pokemon name 
      (Ptype.makeType h [], Some (Ptype.makeType t [])) moves attr None
  | _ -> failwith "Error"
