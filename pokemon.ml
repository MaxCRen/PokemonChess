open Moves
open Ptype

exception NotDefinedCorrectly

type t = {
  name: string;
  pokeType: (Ptype.t * Ptype.t option);
  moveSet: Moves.t list;
  (*contains the multiplier for attributes, it is initializes as all 1's*)
  mutable attr_mult: float list;
  (* attributes [hp; attack; defense; speed] *)
  mutable curr_hp: int;
  attributes: float list;
  mutable status : Moves.status option;

  mutable confused: int;
  mutable accuracy: float
}

let make_pokemon n typ mset attr = {
  name = n;
  pokeType = typ;
  moveSet= mset;
  attr_mult = [1.;1.;1.;1.];
  curr_hp = attr |> List.hd |> Pervasives.int_of_float;
  attributes = attr;
  status = None;
  confused = 0;
  accuracy =  1.
}

let get_moves poke = poke.moveSet

let get_name poke = poke.name

let get_type poke = poke.pokeType

let can_raise_stats poke int = 
  if List.nth poke.attributes int >= 4. then false else true

let get_attr poke = 
  let rec get_attr' acc mult attr= 
    match mult, attr with
    |[], [] -> acc
    |multiplier::t1, attribute::t2 -> get_attr' (multiplier*.(attribute)::acc) t1 t2
    | _, _ -> raise NotDefinedCorrectly in

  List.rev (get_attr' [] poke.attr_mult poke.attributes)

let get_mult poke = poke.attr_mult

let get_curr_hp poke = poke.curr_hp

let get_max_health poke = poke.attributes |> List.hd |> Pervasives.int_of_float

let get_status poke = poke.status

let get_confused poke = poke.confused

let get_accuracy poke = poke.accuracy


let change_health poke health = 
  let max_health = get_max_health poke in
  let new_health = health + poke.curr_hp in
  if new_health < 0 then poke.curr_hp <- 0
  else if new_health > max_health then poke.curr_hp <- max_health 
  else poke.curr_hp <- new_health

let change_attr_mult poke mult = poke.attr_mult <- mult

let change_status poke status = poke.status <- status

let change_confusion poke length= poke.confused <- length

let change_accuracy poke amount = poke.accuracy <- poke.accuracy -. amount

(** ALL NECESSARY POKEMON TYPES *)
let electric = Ptype.make_type "Electric" 
    [("Water", 2.0); ("Flying", 2.0); ("Electric", 0.5); ("Grass", 0.5)]

let water = Ptype.make_type "Water" 
    [("Fire", 2.0); ("Water", 0.5); ("Grass", 0.5)]

let grass = Ptype.make_type "Grass" 
    [("Water", 2.0); ("Fire", 0.5); ("Flying", 0.5); ("Grass", 0.5); 
     ("Poison", 0.5)]

let poison = Ptype.make_type "Poison" [("Grass", 2.0); ("Poison", 0.5)]

let fire = Ptype.make_type "Fire" 
    [("Grass", 2.0); ("Fire", 0.5); ("Water", 0.5)]

let flying = Ptype.make_type "Flying" [("Grass", 2.0); ("Electric", 0.5)]

let psychic = Ptype.make_type "Psychic" [("Poison", 2.0); ("Psychic", 0.5)]

let normal = Ptype.make_type "Normal" []

let fighting = Ptype.make_type "Fighting" 
    [("Flying", 0.5); ("Psychic", 0.5); ("Poison", 0.5)]

let ground = Ptype.make_type "Ground"
    [("Electric", 2.0); ("Fire", 2.0); ("Poison", 2.0); ("Grass", 0.5)]

let dark = Ptype.make_type "Dark" [("Psychic", 2.0)]

let dragon = Ptype.make_type "Dragon" []

let ice = Ptype.make_type "Ice" 
    [("Grass", 2.0); ("Flying", 2.0); ("Fire", 0.5); ("Water", 0.5)]

let ghost = Ptype.make_type "Ghost" ["Psychic", 2.0]

(** ALL NECESSARY POKEMON MOVES *)

let thunder_shock ()= Moves.make_move "Thunder Shock" electric 30 "" 40. 1. 0.1 
    false (Some (Condition (Paralyzed (5), 0.05)))

let thunder_wave ()= Moves.make_move "Thunder Wave" electric 20 
    "Paralyzes opponent." 0. 1. 0.1 false (Some (Condition (Paralyzed 5, 1.)))

let quick_attack ()= Moves.make_move "Quick Attack" 
    normal 30 "" 60. 1. 0.1 true None

let hp_grass ()= Moves.make_move "Hidden Power Grass" 
    grass 15 "" 60. 1. 0.1 false None

let hydro_pump ()= Moves.make_move "Hydro Pump" 
    water 5 "" 110. 0.8 0.1 false None

let work_up ()= Moves.make_move "Work Up" 
    normal 30 "" 0. 0. 0. false (Some (Stats ([0.;0.5;0.5;0.], true)))

let earthquake ()= Moves.make_move "Earthquake" 
    ground 10 "" 100. 1. 0.1 false None

let dark_pulse ()= Moves.make_move "Dark Pulse"
    dark 15 "" 80. 1. 0.1 false None

let amnesia ()= Moves.make_move "Amnesia" psychic 20 "" 0. 0. 0. false 
(Some (Stats ([0.;1.;1.;0.], true)))

let giga_drain ()= Moves.make_move "Giga Drain" 
    grass 10 "" 75. 1. 0.1 false (Some (Heal(0.3, false)))

let sleep_powder ()= Moves.make_move "Sleep Powder"
    grass 15 "Lulls opponent to sleep." 0. 0. 0. false (Some (Condition (Sleep 5, 1.)))

let knock_off ()= Moves.make_move "Knock Off"
    dark 20 "" 65. 1. 0.1 false None

let flare_blitz ()= Moves.make_move "Flare Blitz"
    fire 15 "" 120. 1. 0.1 false (Some (Heal(-0.10, true)))

let dragon_claw ()= Moves.make_move "Dragon Claw" 
    dragon 15 "" 80. 1. 0.1 false (Some (Stats ([0.;0.;0.;1.], true)))

let roost ()= Moves.make_move "Roost"
    flying 10 "Restores half of user's HP" 0. 0. 0. false (Some (Heal(0.5, true)))

let thunder ()= Moves.make_move "Thunder"
    electric 10 "" 110. 0.7 0.1 false (Some (Condition (Paralyzed 5, 1.)))

let fire_blast ()= Moves.make_move "Fire Blast"
    fire 5 "" 110. 0.85 0.1 false (Some (Condition (Burned , 0.05)))

let blizzard ()= Moves.make_move "Blizzard"
    ice 5 "" 110. 0.7 0.1 false None

let shadow_ball ()= Moves.make_move "Shadow Ball"
    ghost 15 "" 80. 1. 0.1 false None

let will_o_wisp ()= Moves.make_move "Will-o-Wisp" fire 15 
    "Burns the opponent. Burnt Pokemon lose some HP every turn" 
    0. 0.85 0. false (Some (Condition (Burned , 1.)))

let toxic ()= Moves.make_move "Toxic" poison 10 
    "Poisons the opponent. Poisoned Pokemon lose some of their HP every turn." 
    0. 0.9 0. false (Some (Condition (Poison , 1.)))

let calm_mind ()= Moves.make_move "Calm Mind" psychic 20 "" 0. 0. 0. false 
                      (Some (Stats ([0.;1.0;0.;0.], true)))

(** ALL NECESSARY POKEMON *)
let get_pawn () = make_pokemon "Pikachu" (electric, None) 
    [thunder_shock (); thunder_wave(); quick_attack(); hp_grass()] 
    [180.; 103.; 58.; 166.]

let get_rook () = make_pokemon "Blastoise" (water, None) 
    [hydro_pump(); work_up(); earthquake(); dark_pulse()] [268.; 153.; 184.; 144.]

let get_bishop () = make_pokemon "Venusaur" (grass, Some poison)
    [amnesia(); sleep_powder(); giga_drain(); knock_off()] [270.; 152.; 153.; 148.]

let get_knight () = make_pokemon "Charizard" (fire, Some flying)
    [flare_blitz(); dragon_claw(); earthquake(); roost()] [266.; 155.; 144.; 184.]

let get_queen () = make_pokemon "Mewtwo" (psychic, None)
    [thunder(); fire_blast(); blizzard(); shadow_ball()] [322.; 276.; 166.; 276.]

let get_king () = make_pokemon "Mew" (psychic, None)
    [will_o_wisp(); toxic(); giga_drain(); calm_mind()] [404.; 184.; 256.; 184.]