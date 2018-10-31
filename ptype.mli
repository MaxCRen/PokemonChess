
(**Our representation of pokemon types has a name of the type, and an association
   list of all types that it is effective against or uneffective against. The float 
   value represents how effective this type is against the other.  *)
type t


(**[getEffectives t1 t2] gets the float effective value of type [t1] 
   on type [t2]*)
val get_effective: t -> t -> float

(**[makeType n eff] is the ptype.t with name [n] and effectiveness [eff]*)
val make_type: string -> (string*float) list -> t
