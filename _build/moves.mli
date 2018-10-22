
(* Our representation of a pokemon status. They can either be Poisoned, 
Paralyzed, Slept, Burnt, or Frozen *)
type status
(* Our representation of a move pokemon move. It contains a name, a ptype, a pp
counter, a description, power, and accuracy *)
type t

(* [can_use move] is true if the move still has pp, and is false if it no longer
has pp *)
val can_use: t -> bool

(* [dec_pp move] decreases the pp of the move used *)
val dec_pp: t -> int