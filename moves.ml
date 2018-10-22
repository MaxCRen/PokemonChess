open Ptype

type status = Poison|Paralyzed|Sleep|Burned|Frozen

type effects = Heal of int | Stats of int| Condition of status

type t = {
  name : string;
  ptype : Ptype.t;

  pp: int;
  description: string;

  power: int option;
  acc: int;

  side_effect: effects option
}

let can_use move = if move.pp = 0 then false else true

let dec_pp move = if can_use move then move.pp - 1 else 0