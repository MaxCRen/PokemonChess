open Ptype

type move = {
  name : string;
  ptype : Ptype.t;
  pp: int;
  description: string;
  power: int;
  acc: int
}
