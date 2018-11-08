open Ptype
open Random

type status = 
  | Poison
  | Paralyzed of int
  | Sleep of int
  | Burned
  | Frozen of int

(* Heal can either be recoil or self heal *)
type effects = | Heal of (float*bool) 
               | Stats of (float list)*bool
               | Condition of (status*float)

type t = {
  name : string;
  ptype : Ptype.t;
  mutable pp: int;
  description: string;
  power: float;
  acc: float;
  crit_rate: float;
  priority: bool;
  side_effect: effects option
}

let make_move n typ p desc pow acc crit pri side =
  {
    name = n;
    ptype = typ;
    pp = p;
    description = desc;
    power = pow;
    acc = acc;
    crit_rate = crit;
    priority = pri;
    side_effect = side

  }

(** [has_condition move] is whether [move] has a side effect or not. *)
let has_condition move = 
  match move.side_effect with
  | Some Condition x -> true
  | _ -> false

let can_use move = if move.pp = 0 then false else true

let dec_pp move = if move.pp = 0 then () else move.pp <- move.pp -1

let get_name move = move.name

let get_type move = move.ptype

let get_pp move = move.pp

let get_description move = move.description

let get_power move = move.power

let get_crit move = move.crit_rate

let get_acc move = move.acc

let get_eff move = move.side_effect

let is_priority move = move.priority