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
               | Charge

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

let can_use move = if move.pp = 0 then false else true

let dec_pp move = move.pp <- move.pp -1

let get_name move = move.name

let get_type move = move.ptype

let get_pp move = move.pp

let get_description move = move.description

let get_power move = 
  let ran_num = Random.float 1. in
  if ran_num <= move.crit_rate then move.power*.2. else move.power

let get_acc move = move.acc

let get_eff move = move.side_effect

let is_priority move = move.priority