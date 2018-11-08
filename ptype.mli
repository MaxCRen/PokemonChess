
(** Represents Pokemon type. [effectiveness] describes how effective attacks of
    this type are against other types. 
    Example: if [effectiveness] of [some_type] = 
    [("water", 0.5); ("fire", 2.0)], where [some_type] has type [t], then
    attacks of type [some_type] are 2.0 times effective against Pokemon of
    type [fire] and 0.5 times effective against Pokemon of type [water]. *)
type t


(** [get_effective t1 t2] is how effective attacks of type [t1] are against
    Pokemon of type [t2]. *)
val get_effective: t -> t -> float

(** [make_type n eff] creates a new pokemon type, with name [n] and 
    effectiveness against other types described in [eff]. *)
val make_type: string -> (string*float) list -> t
