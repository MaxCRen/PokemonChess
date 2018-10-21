<<<<<<< HEAD
open ptype

type move = {
  name : string;
  type : ptype;
  pp: int;
  description: string;

=======
open Ptype

type move = {
  name : string;
  ptype : Ptype.t;
  pp: int;
  description: string
>>>>>>> 2e0e7dd89adaef39e7a0746c68ec1d4adccfac88
}
