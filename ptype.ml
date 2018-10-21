module type ptype = sig

  (*Our representation of pokemon types has a name of the type, and an association
    list of all types that it is effective against or uneffective against. The float 
    value represents how effective this type is against the other.  *)
  type t

  (*[getVal pair poke]  is the effectiveness float value of poke in the pair list
    [pair]. If it is standard effectiveness then it is 1, otherwise it the 
    effectiveness found inthe pair list.*)
  val getVal: (string*float) list -> string -> float

  (*[getEffectives t1 t2] gets the float effective value of type [t1] 
    on type [t2]*)
  val getEffective: t -> t -> float
end

module Ptype:ptype = struct

  type t = {
    name: string;
    effectives: (string*float) list
  }

  let rec getVal (pair: (string*float) list) (poke:string)= 
    match pair with
    |[] -> 1.
    |(name, eff)::t -> if name = poke then eff else getVal t poke


  let getEffective (t1: t) (t2: t) = 
    t2.name |> getVal t1.effectives
end