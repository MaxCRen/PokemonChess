open Battle
open Pokemon
open Moves
open Ptype
open Command
open Random

let rand = Random.self_init

let water = makeType "water" [("water", 0.5);("fire", 2.)]

let fire = makeType "fire" [("fire", 0.5);("water", 2.)]

let bubble = make_move ("bubble") (water) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])

let ember = make_move ("ember") (fire) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])


let available_moves = ["ember"]

(* placeholder pokemon until we implement battles in full *)
let squirtle = make_pokemon "Squirtle" (water, None) [bubble] 
                                        [44.;98.;129.;43.] (None)

let charmander = make_pokemon "Charmander" (fire, None) [ember]
                                        [39.;112.;93.;10.] (None)


let battle = Battle.make_battle charmander squirtle

let opponent_move bat = 
  let op_poke = Battle.get_opponent bat in 
  let opponent_moves = Pokemon.get_moves op_poke in
  let op_name = Pokemon.get_name op_poke in 
  let r = Random.int (List.length opponent_moves) in 
  let move = List.nth opponent_moves r in 
  if can_use_move bat (Moves.get_name move) then 
    ANSITerminal.(print_string [red](op_name^" used "^(Moves.get_name move)^"\n\n\n");
    Battle.use_move bat move)
  else print_string "Can Not Use Move\n\n\n\n" 

let rec display_moves moves =
  match moves with
  |[] -> print_string("\n")
  |h::t when ((List.length t) mod 2 = 0) -> 
    ANSITerminal.(print_string[green]("\n\t"^Moves.get_name h)); (display_moves t); 
  |h::t ->  ANSITerminal.(print_string[green]("\t\t\t"^Moves.get_name h)); (display_moves t)

(** [hp_display number pokemon] returns a rough percentage of [number] '*'
    characters that is equivalent to (current_health / max_health) of 
    pokemon [poke] *)
let hp_display num poke = 
  let percentage_health = 
    (float_of_int (get_curr_hp poke)) /. (float_of_int (get_max_health poke)) in
  String.make (int_of_float (percentage_health *. num)) '*'


(** [printed battle] displays information about the battle [battle] such as
    the current pokemon on either side, the current HP of either pokemon,
    and the moveset available to each pokemon. *)
let printed bat = 
  print_string("__________________________________________________________\n");
  let opponent = Battle.get_opponent bat in
  let player = Battle.get_player bat in
  print_string("Pokemon Battle:");
  ANSITerminal.(print_string[red](" \nOpponent's Pokemon: \n")); 
  ANSITerminal.(print_string[red](Pokemon.get_name opponent ^ "\t Health:" ^ 
               (hp_display 50. opponent)));
  let opp_health = get_curr_hp opponent |> Pervasives.string_of_int in
  let opp_max_health = get_max_health opponent |> Pervasives.string_of_int in
  ANSITerminal.(print_string[red](opp_health^"/"^opp_max_health^" hp\nMoves:"));
  ANSITerminal.(print_string[green]("\n\n\n\nPlayer's Pokemon:\n"));
  ANSITerminal.(print_string [green] (Pokemon.get_name player ^ "\t Health: " ^ 
               (hp_display 50. player)));
  let curr_health = get_curr_hp player |> Pervasives.string_of_int in
  let max_health = get_max_health player |> Pervasives.string_of_int in
  ANSITerminal.(print_string[green](curr_health^"/"^max_health^" hp\nMoves:"));
  get_moves player |> display_moves;
  print_string("__________________________________________________________\n")

(** [print_help] displays to the the given output a list of possible
    commands available to the player *) 
let print_help ()=
  print_string("Here are some general rules: \n");
  print_string("Type 'use [move]' to use a move of your pokemons\n");
  print_string("Type 'info [move]/[pokemon] to get information about your pokemon\n");
  print_string("Type 'quit' if you want to quit\n\n\n")

let use_move bat str = 
  if can_use_move bat str then 
  let poke_name = bat |> Battle.get_player |> Pokemon.get_name in
  let move_list = bat |> Battle.get_player |> get_moves in
  let move = get_move_from_str move_list str in
  ANSITerminal.(print_string[green] (poke_name^" used "^(Moves.get_name move)^"\n\n\n"));
  Battle.use_move bat move
  else print_string "Can Not Use Move\n\n\n\n" 

let check_fainted bat = 
  if Pokemon.get_curr_hp (Battle.get_opponent bat) <= 0 then 
    (print_string("Your opponent has fainted!\n\n\n"); exit 0)
  else if Pokemon.get_curr_hp (Battle.get_player bat) <= 0 then
    (print_string ("You have fainted!\n\n\n"); exit 0)
  else (print_string "")


let rec loop bat =
  check_fainted bat ;
  match read_line () with
  | str -> begin
    match Command.parse_phrase str with
    | exception Empty -> 
      ANSITerminal.erase Screen;
      print_string "Please enter a valid command\n\n\n"; loop bat
    | Help -> print_help (); loop bat
    | Info str-> print_string "unimplemented\n\n\n"; loop bat
    | Incorrect -> 
      print_string "Incorrect Command - 
                    Type 'Help' if you need help\n\n\n"; 
      loop bat
    | Use str -> 
      if List.mem str available_moves then
        (ANSITerminal.erase Screen; use_move bat str; opponent_move bat;  printed bat; loop bat)
      else 
       (print_string (str ^ " is not an available move!\n\n\n"); loop bat)
    | Quit -> print_string "Quitting ...\n\n\n"; exit 0
  end

let rec play_game () = 
  ANSITerminal.erase Screen;
  printed battle;
  loop battle






let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Pokemon\n");
  play_game ()

(* Execute the game engine. *)
let () = main ()
