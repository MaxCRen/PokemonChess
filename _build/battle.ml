open Pokemon
open Moves
open Ptype
open Random

(** [exception IllegalMove] is raised if an illegal move is used in a battle *)
exception IllegalMove


(** [exception Charging] is raised if an attempt is made to use a pokemon 
   who is currently charging a move *)
exception Charging

type t = {
  current_poke: Pokemon.t;
  op_poke: Pokemon.t
}

let get_player battle= battle.current_poke

let get_opponent battle = battle.op_poke

(* Creates an instance of our battle *)
let make_battle player opponent={
  current_poke = player;
  op_poke = opponent
} 

let rec get_move_from_str move_lst move_str =
  match move_lst with 
    |[] -> raise IllegalMove
    |h::t when Moves.get_name h = move_str -> h
    |h::t -> get_move_from_str t move_str

(** [can_move move] returns true if the pp for move [move] > 0 *)
let can_move move=
  if Moves.get_pp move = 0  then false else true

let can_use_move battle str =
  let moves_lst = battle.current_poke |> Pokemon.get_moves in
  match get_move_from_str moves_lst str with 
  | exception IllegalMove -> false
  | move -> can_move move 
  


(**[hit poke move] determines whether the pokemon [poke] hits or misses with the
move [move]. It is 0 if it misses, and 1 if it hits*)
let hit poke move = 
  if (Random.float 1.) <= (Pokemon.get_accuracy poke) *. (Moves.get_acc move) 
  then 0.
  else 1.

(** [calc_damge move battle] calculates the amount of damage [move] does to the 
opponent pokemon in the [battle]. Take into account whether or not the move misses
Calculation:
      (20* Move Power * (Pokemon Attack/Opponent Defense)/50)*)
let calc_damage move battle =
  let poke = battle.current_poke in
  let poke_attr = Pokemon.get_attr poke in
  (20.*.(Moves.get_power move)*.(List.nth poke_attr 1)/.(List.nth poke_attr 2))/.50.


(**[calc_effective move poke dam] calculates the effectiveness of move on pokemon
[poke] and applies the necessary mutiliplier to the damage [dam]*)
let calc_effective move poke dam= 
  let move_type = Moves.get_type move in
  match Pokemon.get_type poke with
  | (t1, None) -> (Ptype.getEffective move_type t1)*.dam
  | (t1, Some t2) -> (Ptype.getEffective move_type t1)*.
                    (Ptype.getEffective move_type t2)*.dam

(** [deal_damage dam poke]  Deals [dam] to pokemon [poke] *)
let deal_damage dam poke = 
  let hp = Pokemon.get_curr_hp poke in 
  if hp < dam then 
    Pokemon.change_health poke (-hp)
  else 
    Pokemon.change_health poke (-dam)

(* 
let rec parse_side_effects eff_lst opponent_player =
  match eff_lst with
  |[] -> make_battle player opponent *)

(* uses the move on poke*)
let use_move battle move =
  let dam = battle |> calc_damage move |> calc_effective move battle.op_poke |> 
  Pervasives.int_of_float in deal_damage dam battle.op_poke

  

  
