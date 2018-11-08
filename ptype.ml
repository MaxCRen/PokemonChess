
type t = {
  name: string;
  effectives: (string*float) list
}

(** [get_val pairs type_name] is the effectiveness float value corresponding to
    [type_name] in [pairs]. If [type_name] is not present as a key in [pairs],
     1. is returned. *)
let rec get_val (pairs: (string*float) list) (type_name:string)= 
  match pairs with
  | [] -> 1.
  | (name, eff)::t -> if name = type_name then eff else get_val t type_name

(** [make_type n eff] creates a new pokemon type, with name [n] and 
    effectiveness against other types described in [eff]. *)
let make_type (n:string) (eff:(string*float) list)=
  {
    name = n;
    effectives = eff
  }

(** [get_effective t1 t2] is how effective attacks of type [t1] are against
    Pokemon of type [t2]. *)
let get_effective (t1: t) (t2: t) = 
  t2.name |> get_val t1.effectives



