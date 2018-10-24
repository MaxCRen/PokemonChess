open Moves
open Ptype

exception NotDefinedCorrectly

type t = {
  name: string;
  pokeType: (Ptype.t * Ptype.t option);
  moveSet: Moves.t list;
  (*contains the multiplier for attributes, it is initializes as all 1's*)
  attr_mult: int list;
  (* attributes [hp; attack; defense; speed] *)
  curr_hp: int;
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
  curr_hp = List.hd attr;
  attributes = attr;
  status = stat;
  confused = false;
  accuracy = 1.
}

let get_name poke = poke.name

let get_type poke = poke.pokeType

let can_raise_stats poke int = 
  if List.nth poke.attributes int >= 4 then false else true

let get_attr poke = 
  let rec get_attr' acc mult attr= 
    match mult, attr with
    |[], [] -> acc
    |multiplier::t1, attribute::t2 -> get_attr' (multiplier*attribute::acc) t1 t2
    | _, _ -> raise NotDefinedCorrectly in
  
  get_attr' [] poke.attr_mult poke.attributes

let get_curr_hp poke = poke.curr_hp

let get_status poke = poke.status

let get_confused poke = poke.confused

let get_accuracy poke = poke.accuracy

