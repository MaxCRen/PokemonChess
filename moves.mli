
(** Our representation of a Pokemon status, which can either be Poisoned, 
    Paralyzed, Sleep, Burned, or Frozen *)
type status = Poison | Paralyzed of int | Sleep of int | Burned | Frozen of int

(** Our representation of a move pokemon move. It contains a name, a ptype, a pp
    counter, a description, power, and accuracy *)
type t

(** Our representation of move side effects, which can either heal, change 
    stats, change condition, confuse, cause recoil, or force Pokemon to 
    recharge. *)
type effects = | Heal of (float*bool) 
               | Stats of (float list)*bool
               | Condition of (status*float)

(** [can_use move] is whether or not the user can use [move], i.e. whether or
    not [move] has greater than 0 PP or not, respectively. *)
val can_use: t -> bool

(** [dec_pp move] decreases the pp of [move] by 1, unless said pp is already 
    0. *)
val dec_pp: t -> unit

(** [make move n typ p desc pow acc crit side] creates a new [move] with
    attributes defined by above parameters. *)
val make_move: string -> Ptype.t -> int -> string -> float -> float -> float 
  -> bool -> effects option-> t

(** [get_name move] is the name of the move [move]. *)
val get_name: t -> string

(** [get_pp move] is the pp of the move [move]. *)
val get_pp: t -> int

(** [get_type move] is the type of the move [move]. *)
val get_type: t -> Ptype.t

(** [get_description move] is the description of the move [move]. *)
val get_description: t -> string

(** [get_power move] is the power of the move [move], 
    accounting for a possible critical hit. *)
val get_power: t -> float

(** [get_acc move] is the accuracy of the move [move]. *)
val get_acc: t -> float

(** [get_eff move] is the list of side effects of the move [move]. *)
val get_eff: t -> effects option

(** [is_priority move] is true if the move [move] is a priority move, and false
    otherwise. *)
val is_priority: t -> bool

(** [has_condition move] is true if the move [move] has a condition effect, and 
    false otherwise. *)
val has_condition: t -> bool

(** [get_crit move] is the possibility that the move [move] will be a critical
    hit. *)
val get_crit: t -> float
