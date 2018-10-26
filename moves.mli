
(** Our representation of a pokemon status. They can either be Poisoned, 
Paralyzed, Slept, Burnt, or Frozen *)
type status = Poison|Paralyzed of int|Sleep of int|Burned|Frozen of int

(** Our representation of a move pokemon move. It contains a name, a ptype, a pp
counter, a description, power, and accuracy *)
type t
(**Our representation of move side effects, the can either heal, change stats
change condition, confuse, have recoil, or charge *)
type effects = |Heal of (float*bool) 
            | Stats of (float list)*bool
            | Condition of (status*float)
            | Confusion of (float*int)
            | Recoil of float
            | Charge of int

(** [can_use move] is true if the move still has pp, and is false if it no longer
has pp *)
val can_use: t -> bool

(** [dec_pp move] decreases the pp of the move used *)
val dec_pp: t -> unit

(**[make move n typ p desc pow acc crit side] creates our representation of a move type*)
val make_move: string -> Ptype.t -> int -> string -> float -> float -> float -> effects list -> t

(**[get_name move] is the name of the move [move]*)
val get_name: t -> string

(**[get_pp move] is the pp of the move [move]*)
val get_pp: t -> int

(**[get_type move] is the type of the move*)
val get_type: t-> Ptype.t

(**[get_description move] is the description of the [move]*)
val get_description: t -> string

(**[get_power move] is the power of the move, accounting for a critical hit*)
val get_power: t -> float

(**[gett_acc move] is the accuracy of the move*)
val get_acc: t -> float

(**[get_eff move] is the list of side effects of the [move]*)
val get_eff: t-> effects list
