
type t = {
  name: string;
  effectives: (string*float) list
}

let rec getVal (pair: (string*float) list) (poke:string)= 
  match pair with
  |[] -> 1.
  |(name, eff)::t -> if name = poke then eff else getVal t poke

let makeType (n:string) (eff:(string*float) list)=
{
  name = n;
  effectives = eff
}

let getEffective (t1: t) (t2: t) = 
  t2.name |> getVal t1.effectives



