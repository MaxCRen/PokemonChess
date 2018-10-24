open Battle
open Pokemon
open Moves
open Ptype
open Command

let water = makeType "water" [("water", 0.5);("fire", 2.)]
let fire = makeType "fire" [("fire", 0.5);("water", 2.)]

let bubble = make_move ("bubble") (water) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])

let ember = make_move ("ember") (fire) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])


let squirtle = make_pokemon "Squirtle" (water, None) [bubble] 
                                        [44.;98.;129.;43.] (None)

let charmander = make_pokemon "Charmander" (fire, None) [ember]
                                        [39.;112.;93.;10.] (None)

let battle = Battle.make_battle charmander squirtle

let rec display_moves moves =
  match moves with
  |[] -> print_string("\n")
  |h::t when ((List.length t) mod 2 = 0) -> 
                print_string("\n\t"^Moves.get_name h); (display_moves t); 
  |h::t ->  print_string("\t\t\t"^Moves.get_name h); (display_moves t)


let printed bat = 
  print_string("__________________________________________________________\n");
  let opponent = Battle.get_opponent bat in
  let player = Battle.get_player bat in
  print_string("Pokemon Battle: \nOpponent's Pokemon: \n"); 
  print_string(Pokemon.get_name opponent ^ "\t Health: ********************");
  let opp_health = get_curr_hp opponent |> Pervasives.string_of_int in
  let opp_max_health = get_max_health opponent |> Pervasives.string_of_int in
  print_string(opp_health^"/"^opp_max_health^" hp\nMoves:");
  print_string("\n\n\n\nPlayer's Pokemon:\n");
  print_string(Pokemon.get_name player ^ "\t Health: ******************** ");
  let curr_health = get_curr_hp player |> Pervasives.string_of_int in
  let max_health = get_max_health player |> Pervasives.string_of_int in
  print_string(curr_health^"/"^max_health^" hp\nMoves:");
  get_moves player |> display_moves;
  print_string("__________________________________________________________\n")

let print_help ()=
  print_string("Here are some general rules: \n");
  print_string("Type 'use [move]' to use a move of your pokemons\n");
  print_string("Type 'info [move]/[pokemon] to get information about your pokemon\n");
  print_string("Type 'quit' if you want to quit\n\n\n")




let use_move bat str = 
  if true then 
  let poke_name = bat |> Battle.get_player |> Pokemon.get_name in
  let move_list = bat |> Battle.get_player |> get_moves in
  let move = get_move_from_str move_list str in
  print_string (poke_name^" used "^(Moves.get_name move)^"\n\n\n");
                        Battle.use_move bat move;
  else print_string "Can Not Use Move\n\n\n\n" 







let rec loop bat =
  match read_line () with
  | str -> begin
    match Command.parse_phrase str with
    | exception Empty -> print_string "Please enter a valid command\n\n\n"; loop bat
    | Help -> print_help (); loop bat
    | Info str-> print_string"unimplemented\n\n\n"; loop bat
    | Incorrect -> print_string "Incorrect Command - Type 'Help' if you need help\n\n\n"; loop bat
    | Use str -> use_move bat str; printed bat; loop bat
    | Quit -> print_string "Quitting ...\n\n\n"; exit 0
    
  end

let rec play_game () = 
  printed battle;
  loop battle






let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Pokemon\n");
  play_game ()

(* Execute the game engine. *)
let () = main ()