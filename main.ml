open Battle
open Pokemon
open Moves
open Ptype
open Command
open Random


(**[print_logo ()] is used to print the ascii art of the logo for pokemon,
ascii drawing courtesy of https://www.asciiart.eu/video-games/pokemon*)
let print_logo () = 
  ANSITerminal.(print_string [yellow]
                  "_________________________________________________________________________                                  
|                                  ,'\\                                  |
|   _.----.        ____          ,'  _\\   ___    ___     ____           |
|_,-'       `.     |    |  /`.   \\,-'    |   \\  /   |   |    \\  |`.     |
|\\      __    \\    '-.  | /   `.  ___    |    \\/    |   '-.   \\ |  |    |
| \\.    \\ \\   |  __  |  |/    ,','_  `.  |          | __  |    \\|  |    |
|   \\    \\/   /,' _`.|      ,' / / / /   |          ,' _`.|     |  |    |
|    \\     ,-'/  /   \\    ,'   | \\/ / ,`.|         /  /   \\  |     |    |
|     \\    \\ |   \\_/  |   `-.  \\    `'  /|  |    ||   \\_/  | |\\    |    |
|      \\    \\ \\      /       `-.`.___,-' |  |\\  /| \\      /  | |   |    |
|       \\    \\ `.__,'|  |`-._    `|      |__| \\/ |  `.__,'|  | |   |    |
|        \\_.-'       |__|    `-._ |              '-.|     '-.| |   |    |
|                                `'                            '-._|    |\n")

let rand = Random.self_init

let water = makeType "water" [("water", 0.5);("fire", 2.)]

let fire = makeType "fire" [("fire", 0.5);("water", 0.5)]

let bubble = make_move ("Bubble") (water) (25) 
    ("Does damage") (30.) (1.) (0.10) false None

let random2 = make_move ("Speed") (water) (25) 
    ("Does damage") (0.) (1.) (0.) false (Some (Stats ([0.;0.;0.;1.0], true)))

let ember = make_move ("Ember") (fire) (25) 
    ("Does damage") (30.) (1.) (0.10) false None

(** [get_move_names moves] is a list of the names of all moves in [moves], with
    each name in all lowercase. *)
let get_move_names moves =
  List.map (fun x -> Moves.get_name x) moves

(* placeholder pokemon until we implement battles in full *)
let squirtle = Pokemon.make_pokemon "Squirtle" (water, None) [bubble; random2] 
    [44.;98.;129.;43.] 

let charmander = Pokemon.make_pokemon "Charmander" (fire, None) [ember]
    [39.;112.;93.;55.] 


let battle = Battle.make_battle squirtle charmander

let print_eff move poke = 
  match Battle.calc_effective move poke with
  | x -> if x >= 2. then print_string "It's super effective!\n\n\n"
    else if x < 0. then print_string "It's not very effective...\n\n\n"
    else if x = 0. then print_string "But nothing happened...\n\n\n"

(** [hp_display number pokemon] returns a rough percentage of [number] '*'
    characters that is equivalent to (current_health / max_health) of 
    pokemon [poke] *)
let hp_display num poke = 
  let percentage_health = 
    (float_of_int (get_curr_hp poke)) /. (float_of_int (get_max_health poke)) in
  String.make (int_of_float (percentage_health *. num)) '*'

(** display_moves[moves] displays all of the moves available to the player's
    pokemon *)
let rec display_moves moves =
  match moves with
  | [] -> print_string("\n")
  | h::t when ((List.length t) mod 2 = 0) -> 
    ANSITerminal.(print_string[green]("\n\t"^Moves.get_name h)); 
    (display_moves t);
  | h::t ->  ANSITerminal.(print_string[green]("\t"^Moves.get_name h)); 
    (display_moves t)

(** [printed battle] displays information about the battle [battle] such as
    the current Pokemon on either side, the current HP of either Pokemon,
    and the moveset available to each Pokemon. *)
let printed bat = 
  print_logo ();
  ANSITerminal.(print_string [yellow] ("|_______________________________________________________________________|\n"));
  let opponent = Battle.get_opponent bat in
  let player = Battle.get_player bat in
  ANSITerminal.(print_string[red](" \nOpponent's Pokemon: \t\t\t\t")); 
  ANSITerminal.(print_string[red](Pokemon.get_name opponent ^ "\n\t\t\tHealth:" ^ 
                                  (hp_display 35. opponent) ^ "\n\t\t\t"));
  let opp_health = get_curr_hp opponent |> Pervasives.string_of_int in
  let opp_max_health = get_max_health opponent |> Pervasives.string_of_int in
  ANSITerminal.(print_string[red](opp_health^"/"^opp_max_health^" hp"));
  ANSITerminal.(print_string[green]("\n\n\n\nPlayer's Pokemon:\t\t\t\t"));
  ANSITerminal.(print_string [green] 
                  (Pokemon.get_name player ^ "\n\t\t\tHealth:" 
                   ^ (hp_display 35. player) ^ "\n\t\t\t"));
  let curr_health = get_curr_hp player |> Pervasives.string_of_int in
  let max_health = get_max_health player |> Pervasives.string_of_int in
  ANSITerminal.(print_string[green](curr_health^"/"^max_health^" hp\nMoves:"));
  Pokemon.get_moves player |> display_moves;
  print_string("_________________________________________________________________________\n")

(**[check_fainted bat] checks the battle for if either of the pokemon has fainted
   and then performs the correct action accordingly.*)
let check_fainted bat = 
  if Pokemon.get_curr_hp (Battle.get_opponent bat) <= 0 then 
    (print_string("Your opponent has fainted!\n\n\n"); exit 0)
  else if Pokemon.get_curr_hp (Battle.get_player bat) <= 0 then
    (print_string ("You have fainted!\n\n\n"); exit 0)
  else (print_string "")

let opponent_move bat = 
  let op_poke = Battle.get_opponent bat in 
  let opponent_moves = Pokemon.get_moves op_poke in
  let op_name = Pokemon.get_name op_poke in 
  let r = Random.int (List.length opponent_moves) in 
  let move = List.nth opponent_moves r in 
  if Battle.can_move move then 
    (* Sequence of events: use move -> print the new state -> print action ->
       print effectiveness -> check if opponent has fainted *)
    (Battle.use_move bat move; printed bat;
     ANSITerminal.(print_string [red](op_name^" used "^(Moves.get_name move)^"!\n");
                   print_eff move (Battle.get_opponent bat); check_fainted bat ;))

  else print_string "Cannot use move\n\n\n\n" 



(** [print_help] displays to the the given output a list of possible
    commands available to the player *) 
let print_help ()=
  print_string("Here are some general rules: \n");
  print_string("Type 'use [move]' to use a move of your pokemons\n");
  print_string("Type 'info [move]/[pokemon] to get information about your pokemon\n");
  print_string("Type 'quit' if you want to quit\n\n\n")


(**[use_move str bat] takes the string representation of a move [str] and then
   applies it to the battle [bat] by either doing damage or some other effect*)
let use_move move bat= 
  if Battle.can_move move then (
    let poke_name = bat |> Battle.get_player |> Pokemon.get_name in
    
    (* Sequence of events: use move -> print the new state -> print action ->
       print effectiveness -> check if opponent has fainted *)
    Battle.use_move bat move; printed bat;
    ANSITerminal.(print_string[green] (poke_name^" used "^(Moves.get_name move)^"\n"));
    print_eff move (Battle.get_opponent bat); check_fainted bat; )
  else (printed bat; print_string "Cannot use move\n\n\n\n" )




(*Likely refactor these two methods later, but for now Leave as is.*)
(**[move_first bat str] takes in the battle and the move of the player [str] and
   peforms the move of the pokemon that moves first*)
let move_first bat str= 
  let move_list = bat |> Battle.get_player |> get_moves in
  let move = get_move_from_str move_list str in
  let player = Battle.get_player bat in
  let opponent = Battle.get_opponent bat in
  if (Moves.is_priority move) then use_move move bat else
  if (compare_speed player opponent) = player then use_move move bat else 
    opponent_move bat

(**[move_second bat str] takes in the battle and the move of the player [str] and
   peforms the move of the pokemon that moves second*)
let move_second bat str = 
  let move_list = bat |> Battle.get_player |> get_moves in
  let move = get_move_from_str move_list str in
  let player = Battle.get_player bat in
  let opponent = Battle.get_opponent bat in
  if (Moves.is_priority move) then opponent_move bat else
  if (compare_speed player opponent) = player then opponent_move bat else 
    use_move move bat

(* [pause_bool] is used to check if we are in the enter anything to continue state
   or if we are in parsing the player's move state. last_move is the last move
   used by the player so we can use it if the player moves second *)
let rec loop bat pause_bool last_move=
  let available_moves = get_move_names 
      (Pokemon.get_moves (Battle.get_player bat)) in
  if pause_bool then 
    (print_string "Enter anything to continue";
     match read_line () with
     |_ ->  ANSITerminal.erase Screen; move_second bat last_move; loop bat false last_move)
  else
    (* Parses actions that the player may want to take*)
    match read_line () with
    | str -> begin
        match Command.parse_phrase str with
        (* Empty then we just deal with a command *)
        | exception Empty -> 
          ANSITerminal.erase Above;
          print_string "Please enter a valid command\n\n\n"; loop bat false last_move
        | Help -> print_help (); loop bat false last_move
        | Info str-> print_string "unimplemented\n\n\n"; loop bat false last_move
        | Incorrect ->  
          print_string "Incorrect Command - 
                    Type 'Help' if you need help\n\n\n"; 
          loop bat false last_move
        | Use str -> 
          if List.mem str available_moves then
            (ANSITerminal.erase Screen; move_first bat str; loop bat true str;)
          else 
            (print_string (str ^ " is not an available move!\n\n\n"); 
             loop bat false last_move)
        | Quit -> print_string "Quitting ...\n\n\n"; exit 0
      end

let play_game () = 
  printed battle;
  print_string "You entered a battle with a pokemon. Fight to stay alive\n";
  loop battle false ""


let main () =
  ANSITerminal.erase Screen;
  play_game ()

(* Execute the game engine. *)
let () = main ()
