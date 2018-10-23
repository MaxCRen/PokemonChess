open Ptype

type status = Normal|Poison|Paralyzed|Sleep|Burned|Frozen

(*Heal: how much to heal; Stats: int refers to which attribute will be changed,
and float is how much the multiplier is increased or decreased. Condition is 
the change in status. *)
type effects = Heal of float 
            | Stats of (int*float) list
            | Condition of status
            | Confusion of bool
            | Flinch of float
            | Multiple of int
            | Recoil of int

type t = {
  name : string;
  ptype : Ptype.t;

  pp: int;
  description: string;

  power: int option;
  acc: int;
  crit_rate: float;

  side_effect: effects option list
}

let can_use move = if move.pp = 0 then false else true

let dec_pp move = if can_use move then move.pp - 1 else 0