open Moves
open Ptype

exception NotDefinedCorrectly

type t = {
  name: string;
  pokeType: (Ptype.t * Ptype.t option);
  mutable moveSet: Moves.t list;
  (*contains the multiplier for attributes, it is initializes as all 1's*)
  mutable attr_mult: float list;
  (* attributes [hp; attack; defense; speed] *)
  mutable curr_hp: int;
  attributes: float list;
  mutable status : Moves.status option;

  mutable confused: bool;
  mutable accuracy: float
}

let make_pokemon n typ mset attr stat = {
  name = n;
  pokeType = typ;
  attr_mult = [1.;1.;1.;1.];
  moveSet= mset;
  curr_hp = attr |> List.hd |> Pervasives.int_of_float;
  attributes = attr;
  status = stat;
  confused = false;
  accuracy =  1.
}

let get_moves poke = poke.moveSet

let get_name poke = poke.name

let get_type poke = poke.pokeType

let can_raise_stats poke int = 
  if List.nth poke.attributes int >= 4. then false else true

let get_attr poke = 
  let rec get_attr' acc mult attr= 
    match mult, attr with
    |[], [] -> acc
    |multiplier::t1, attribute::t2 -> get_attr' (multiplier*.(attribute)::acc) t1 t2
    | _, _ -> raise NotDefinedCorrectly in
  
  get_attr' [] poke.attr_mult poke.attributes

let get_mult poke = poke.attr_mult

let get_curr_hp poke = poke.curr_hp

let get_status poke = poke.status

let get_confused poke = poke.confused

let get_accuracy poke = poke.accuracy

let change_health poke health = poke.curr_hp <- poke.curr_hp + health



