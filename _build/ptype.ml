type ptype = {
  name: string;
  effectives: (string*float) list
}

let fire = {name = "Fire"; effectives = [("Fire", 1.)]}

exception NotFound of string

let rec getVal (pair: (string*float) list) (poke:string)= 
  match pair with
  |[] -> raise (NotFound "this type is not found in list")
  |(name, eff)::t -> if name = poke then eff else getVal t poke

let getEffective (t1: ptype) (t2: ptype) = 
  t2.name |> getVal t1.effectives
