open Ptype
open Random

type status = Normal|Poison|Paralyzed of int|Sleep of int|Burned|Frozen of int


type effects = Heal of float 
            | Stats of (int*float) list
            | Condition of (status*float)
            | Confusion of (bool*float)
            | Recoil of int
            | Charge of int

type t = {
  name : string;
  ptype : Ptype.t;

  pp: int;
  description: string;

  power: int;
  acc: float;
  crit_rate: float;

  side_effect: effects list
}

let make_move n typ p desc pow acc crit side =
{
  name = n;
  ptype = typ;

  pp = p;
  description = desc;

  power = pow;
  acc = acc;
  crit_rate = crit;

  side_effect = side

}

let can_use move = if move.pp = 0 then false else true

let dec_pp move = if can_use move then move.pp - 1 else 0

let get_name move = move.name

let get_type move = move.ptype

let get_pp move = move.pp

let get_description move = move.description

let get_power move = 
    let ran_num = Random.float 1. in
    if ran_num <= move.crit_rate then move.power*2 else move.power
  
let get_acc move = move.acc

let get_eff move = move.side_effect


