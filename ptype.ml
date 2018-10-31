
type t = {
  name: string;
  effectives: (string*float) list
}

let rec get_val (pair: (string*float) list) (poke:string)= 
  match pair with
  |[] -> 1.
  |(name, eff)::t -> if name = poke then eff else get_val t poke

let make_type (n:string) (eff:(string*float) list)=
  {
    name = n;
    effectives = eff
  }

let get_effective (t1: t) (t2: t) = 
  t2.name |> get_val t1.effectives



