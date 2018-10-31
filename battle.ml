open Pokemon
open Moves
open Ptype
open Random

(** [exception IllegalMove] is raised if an illegal move is used in a battle *)
exception IllegalMove


(** [exception Charging] is raised if an attempt is made to use a pokemon 
    who is currently charging a move *)
exception Charging of int

type t = {
  current_poke: Pokemon.t;
  op_poke: Pokemon.t;
  mutable turn : Pokemon.t
}

let get_player battle= battle.current_poke

let get_opponent battle = battle.op_poke

let compare_speed player opponent =
  let op_speed = List.nth (opponent |> Pokemon.get_attr) 3 in
  let pl_speed = List.nth (player |> Pokemon.get_attr) 3 in
  if op_speed >= pl_speed then opponent else player

(* Creates an instance of our battle *)
let make_battle player opponent = {
  current_poke = player;
  op_poke = opponent;
  turn = compare_speed player opponent
} 

(*[other_player bat] is the player opposite to whose turn it is. This is necessary
  for damage calculations when we need to know who the opposite player recieving the damage is*)
let other_player bat =
  if bat.turn = bat.current_poke then bat.op_poke
  else bat.current_poke

let rec get_move_from_str move_lst move_str =
  match move_lst with 
  | [] -> raise IllegalMove
  | h::t when Moves.get_name h = move_str -> h
  | h::t -> get_move_from_str t move_str

(** [can_move move] returns true if the pp for move [move] > 0 *)
let can_move move=
  if Moves.get_pp move = 0  then false else true

(**[hit poke move] determines whether the pokemon [poke] hits or misses with the
   move [move]. It is 0 if it misses, and 1 if it hits*)
let hit poke move = 
  if (Random.float 1.) <= (Pokemon.get_accuracy poke) *. (Moves.get_acc move) 
  then 0.
  else 1.


let calc_effective move poke= 
  let move_type = Moves.get_type move in
  match Pokemon.get_type poke with
  | (t1, None) -> (Ptype.get_effective move_type t1)
  | (t1, Some t2) -> (Ptype.get_effective move_type t1) *.
                     (Ptype.get_effective move_type t2)
(** [calc_damge move battle] calculates the amount of damage [move] does to the 
    opponent pokemon in the [battle]. Take into account whether or not the move misses
    Calculation:
      type_Mult *(20* Move Power * (Pokemon Attack/Opponent Defense)/50)*)
let calc_damage move battle =
  (*The pokemon we are doing damage to*)
  let curr_poke = battle.turn in
  let curr_poke_atk = List.nth (Pokemon.get_attr curr_poke) 1  in
  let other_poke = other_player battle in
  let other_poke_def = List.nth (Pokemon.get_attr other_poke) 2  in
  let type_mult = calc_effective move other_poke in
  type_mult*.(20.*.(Moves.get_power move)*.(curr_poke_atk)/.(other_poke_def))/.50.


(**[calc_effective move poke dam] calculates the effectiveness of move on pokemon
   [poke] and applies the necessary mutiliplier to the damage [dam]*)



(** [deal_damage dam poke]  Deals [dam] to pokemon [poke] *)
let deal_damage dam poke = 
  Pokemon.change_health poke (-dam)

exception AttributeSizesDifferent

(*[deal_with_attribute_changes acc poke1 lst] goes through the stat changes in lst
  and applies them to pokemon, for example, for a Stats changer of [0, 0.5, 0.5, 0]
  the stat multipliers of attack and defense will increase by 0.5. In order for this
  to be correct the pokemon's attributes must be the same length as the pokemon
  multiplier. We can assume this because this is how our convention*)
let deal_with_attr poke1 lst =
  let poke1_mult = Pokemon.get_mult poke1 in
  let rec attribute_changer acc p_mult lst =
    match p_mult, lst with 
    |[], [] -> acc
    |atr::poke_lst, mult::changer_lst -> 
      attribute_changer (atr+.mult::acc) poke_lst changer_lst
    |_,_ -> failwith "Can't Happen" in
  attribute_changer [] poke1_mult lst

(**[calc_chances perc] is randomly true or false. It is true [perc] percent
   of the time and false all other times*)
let calc_chances perc =
  let num = Random.float 1. in
  if num <= perc then true else false

(**[apply_condition poke1 condition perc] applies the status change of [condition]
   to pokemon [poke1], [perc] a valid percentage in decimal form percent of the time.*)
let apply_condition poke1 condition perc =
  if calc_chances perc then
    Pokemon.change_status poke1 (Some condition)
  else ()


(**[apply_effect effect poke1 poke2] applies the effect of a move by [poke1]
   onto [poke2] or itself, with [dam] used for health and recoil, as how much one
   heals or takes recoil is dependent on the amount of damage done. if the effect
   is Charge, then it is the only effect in the list*)
let apply_effect effect poke1 poke2 dam= 
  match effect with
  (*takes [perc] of health from which, if which is true then takes perc health
    from max health, if false it takes [perc] of health from damage done*)
  |Heal(perc,player) when player -> 
    let max_health = (Pokemon.get_max_health poke1) |> float_of_int in 
    deal_damage  (-(max_health*.perc|>int_of_float)) poke1
  |Heal (perc, _) -> 
    let gained = float_of_int dam in
    deal_damage (-(gained*.perc|>int_of_float)) poke1
  (*which attribute we are changing, change by how much, if player then we are
    applying the change to poke1 otherwise apply to poke2*)
  |Stats (lst,player) when player-> lst |> deal_with_attr poke1 
                                    |> Pokemon.change_attr_mult poke1
  |Stats (lst, _) -> lst |> deal_with_attr poke2 
                     |> Pokemon.change_attr_mult poke2
  |Condition (stat, perc) -> apply_condition poke2 stat perc

(**[parse_side_effects eff_lst bat dam] goes through the list of side effects
   and applies the effects to the battle [bat]*)
let parse_side_effects eff bat dam=
  match eff with
  |None -> ()
  |Some effect -> apply_effect effect bat.turn (other_player bat) dam

(* uses the move on poke*)
let use_move battle move =
  let dam = battle |> calc_damage move |> Pervasives.int_of_float in 
  deal_damage dam (other_player battle);
  parse_side_effects (Moves.get_eff move) battle dam

let change_turn battle poke = battle.turn <- poke

let get_turn battle = battle.turn

