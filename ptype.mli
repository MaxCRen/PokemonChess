
  (**Our representation of pokemon types has a name of the type, and an association
  list of all types that it is effective against or uneffective against. The float 
  value represents how effective this type is against the other.  *)
  type t

  (**[getVal pair poke]  is the effectiveness float value of poke in the pair list
  [pair]. If it is standard effectiveness then it is 1, otherwise it the 
  effectiveness found inthe pair list.*)
  val getVal: (string*float) list -> string -> float

  (**[getEffectives t1 t2] gets the float effective value of type [t1] 
  on type [t2]*)
  val getEffective: t -> t -> float

  (**[makeType n eff] is the ptype.t with name [n] and effectiveness [eff]*)
  val makeType: string -> (string*float) list -> t
