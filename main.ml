open Battle
open Pokemon
open Moves
open Ptype
open Command

let water = Ptype.makeType "water" [("water", 0.5);("fire", 2.)]
let fire = Ptype.makeType "fire" [("fire", 0.5);("water", 2.)]

let bubble = Moves.make_move ("bubble") (water) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])

let ember = Moves.make_move ("ember") (fire) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])


let squirtle = Pokemon.make_pokemon "Squirtle" (water, None) [bubble] 
                                        [22.;30.;40.;10.] (None)

let charmander = Pokemon.make_pokemon "Charmander" (fire, None) [ember]
                                        [22.;30.;40.;10.] (None)

let battle = Battle.make_battle squirtle charmander

let rec display_moves moves =
  match moves with
  |[] -> print_string("\n")
  |h::t when ((List.length t) mod 2 = 0) -> 
                print_string("\n\t"^Moves.get_name h); (display_moves t); 
  |h::t ->  print_string("\t\t\t"^Moves.get_name h); (display_moves t)


let printed bat = 
  let opponent = Battle.get_opponent bat in
  let player = Battle.get_player bat in
  print_string("Pokemon Battle: \nOpponent's Pokemon: \n"); 
  print_string(Pokemon.get_name opponent ^ "\t Health: ********************");
  print_string("\n\n\n\nPlayer's Pokemon:\n");
  print_string(Pokemon.get_name player ^ "\t Health: ******************** ");
  let curr_health = Pokemon.get_curr_hp player |> Pervasives.string_of_int in
  let max_health = Pokemon.get_max_health player |> Pervasives.string_of_int in
  print_string(curr_health^"/"^max_health^" hp\nMoves:");
  Pokemon.get_moves player |> display_moves


let rec loop bat =
  match read_line () with
  | str ->
    match Command.parse_phrase str with

let rec play_game () = 
  printed battle






let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Pokemon.\n");
  play_game ()

(* Execute the game engine. *)
let () = main ()