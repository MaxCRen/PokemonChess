open Battle
open Pokemon
open Moves
open Ptype
open Command
open Random
open Chess
open ChessGame

let new_chess_game = ChessGame.new_game

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

let water = make_type "water" [("water", 0.5);("fire", 2.)]

let fire = make_type "fire" [("fire", 0.5);("water", 0.5)]

let bubble = make_move ("Bubble") (water) (25) 
    ("Does damage") (30.) (1.) (0.10) false None

let random2 = make_move ("Sleep") (water) (25) 
    ("Does damage") (0.) (1.) (0.) false (Some (Condition (Sleep (1), 1.)))

let ember = make_move ("Ember") (fire) (25) 
    ("Does damage") (30.) (1.) (0.10) false None

(** [get_move_names moves] is a list of the names of all moves in [moves], with
    each name in all lowercase. *)
let get_move_names moves =
  List.map (fun x -> Moves.get_name x |> String.lowercase_ascii) moves

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


let display_status poke = 
  match (Pokemon.get_status poke) with
  | Some Poison -> ANSITerminal.(print_string[green] "[PSN]")
  | Some Paralyzed _ -> ANSITerminal.(print_string[green] "[PAR]")
  | Some Sleep _-> ANSITerminal.(print_string[yellow] "[SLP]")
  | Some Burned -> ANSITerminal.(print_string[red] "[BUR]")
  | Some Frozen _-> ANSITerminal.(print_string[cyan] "[FRZ]")
  | _ -> ()

(** *)
let print_chess = true

(** [printed btl] displays information about the battle [btl] such as the
    current Pokemon on either side, the current HP of both Pokemon,
    and the moveset available to each Pokemon. *)
let printed btl = 
  print_logo ();
  ANSITerminal.(print_string [yellow] ("|_______________________________________________________________________|\n"));
  let opponent = Battle.get_opponent btl in
  let player = Battle.get_player btl in

  ANSITerminal.(print_string[red]("\nOpponent's Pokemon: \t\t")); display_status opponent;
  ANSITerminal.(print_string[red](Pokemon.get_name opponent ^ "\n\t\t\tHealth:" ^ 
                                  (hp_display 35. opponent) ^ "\n\t\t\t"));
  let opp_health = get_curr_hp opponent |> Pervasives.string_of_int in
  let opp_max_health = get_max_health opponent |> Pervasives.string_of_int in
  ANSITerminal.(print_string[red](opp_health^"/"^opp_max_health^" hp"));
  ANSITerminal.(print_string[green]("\n\n\n\nPlayer's Pokemon:\t\t"));display_status player;
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


(** [print_help] displays to the the given output a list of possible
    commands available to the player *) 
let print_help ()=
  print_string("Here are some general rules: \n");
  print_string("Type 'use [move]' to use a move of your pokemons\n");
  print_string("Type 'info [move]/[pokemon] to get information about your pokemon\n");
  print_string("Type 'quit' if you want to quit\n\n\n")


(*[conditions_helper poke status] matches the correct condition effect with 
  what it does to the pokemon. (i.e.) poison hurts the pokemon and sleep puts
  the pokemon to sleep*)
let conditions_helper poke status =
  let max_health = Pokemon.get_max_health poke |> Pervasives.float_of_int in
  match status with 
  | Poison ->  Battle.deal_damage (Pervasives.int_of_float (0.06*.max_health)) poke;
    print_string ((Pokemon.get_name poke)^" was hurt by poison\n")
  | Paralyzed x-> if x - 1 = 0 then (print_string "Paralysis wore off\n";
                                     Pokemon.change_status poke None)
    else Pokemon.change_status poke (Some (Paralyzed (x-1)))
  | Sleep x -> if x - 1 = 0 then 
      (print_string ((Pokemon.get_name poke)^" woke up!\n");
       Pokemon.change_status poke None)
    else Pokemon.change_status poke (Some (Sleep (x-1)))
  | Burned -> print_string ((Pokemon.get_name poke)^" was burned!\n");
    Battle.deal_damage (Pervasives.int_of_float (0.06*.max_health)) poke
  | Frozen x -> failwith "Coming soon"

(**[deal_with_conditions bat] deals with the pokemon's conditions after every
   turn*)
let deal_with_conditions bat =
  let player = Battle.get_player bat in
  let opponent = Battle.get_opponent bat in
  match (Pokemon.get_status player), (Pokemon.get_status opponent) with
  | None, None -> ()
  | Some stat, None -> conditions_helper player stat
  | None, Some stat -> conditions_helper opponent stat
  | Some stat1, Some stat2 -> conditions_helper player stat1; 
    conditions_helper opponent stat2


let pause_bet_states battle func move =
  print_string "Enter anything to continue";
  match read_line () with
  |_ ->  (ANSITerminal.erase Screen; func move battle)

(**[use_move str bat] takes the string representation of a move [str] and then
   applies it to the battle [bat] by either doing damage or some other effect*)
let use_move move bat= 
  if Battle.can_move move then (
    let poke_name = bat |> Battle.get_turn |> Pokemon.get_name in

    (* Sequence of events: use move -> print the new state -> print action ->
       print effectiveness -> check if opponent has fainted *)
    Battle.use_move bat move;


    bat |> Battle.other_player |> Battle.change_turn bat;
    printed bat;
    (if Battle.get_turn bat = Battle.get_opponent bat then (
        ANSITerminal.(print_string[green] (poke_name^" used "^(Moves.get_name move)^"\n"));
        print_eff move (Battle.other_player bat); check_fainted bat;)
     else
       ANSITerminal.(print_string[red] (poke_name^" used "^(Moves.get_name move)^"\n"));
     print_eff move (Battle.other_player bat); check_fainted bat;)
  )
  else (printed bat; print_string "Cannot use move\n\n\n\n" )

(*Likely refactor these two methods later, but for now Leave as is.*)
(**[move_first bat str] takes in the battle and the move of the player [str] and
   peforms the move of the pokemon that moves first*)
let move_turns bat str= 
  let move_list = bat |> Battle.get_player |> Pokemon.get_moves in
  let move = get_move_from_str move_list str in
  let player = Battle.get_player bat in
  let opponent = Battle.get_opponent bat in
  let opponent_moves = Pokemon.get_moves opponent in
  let op_move = Random.int (List.length opponent_moves) |> List.nth opponent_moves in 

  if (Moves.is_priority move) then (change_turn bat player; use_move move bat;
                                    pause_bet_states bat use_move move)
  else
    let faster = compare_speed player opponent in
    change_turn bat (faster); 
    use_move op_move bat; pause_bet_states bat use_move move


(* (**[move_second bat str] takes in the battle and the move of the player [str] and
   peforms the move of the pokemon that moves second*)
   let move_second bat str = 
   let move_list = bat |> Battle.get_player |> get_moves in
   let move = get_move_from_str move_list str in
   let player = Battle.get_player bat in
   if player = Battle.get_turn bat then use_move move bat else opponent_move bat *)

(* [pause_bool] is used to check if we are in the enter anything to continue state
   or if we are in parsing the player's move state. last_move is the last move
   used by the player so we can use it if the player moves second *)
let rec battle_loop btl =
  let available_moves = get_move_names 
      (Pokemon.get_moves (Battle.get_player btl)) in
  (* Parses actions that the player may want to take*)
  match read_line () with
  | str -> begin
      match Command.parse_phrase str with
      (* Empty then we just deal with a command *)
      | exception Empty -> 
        ANSITerminal.erase Above;
        print_string "Please enter a valid command\n\n\n"; battle_loop btl 
      | Help -> print_help (); battle_loop btl
      | Info str -> print_string "unimplemented\n\n\n"; battle_loop btl
      | Incorrect ->  
        print_string "Invalid Command - 
                  Type 'Help' if you need help\n\n\n"; 
        battle_loop btl
      | Use str2 -> 
        if List.mem (String.lowercase_ascii str2) available_moves then
          let move_list = btl |> Battle.get_player |> Pokemon.get_moves in
          let poke_move = Battle.get_move_from_str move_list 
              (String.lowercase_ascii str2) in
          if Battle.can_move poke_move then
            (ANSITerminal.erase Screen; move_turns btl str2; battle_loop btl;)
          else(
            print_string (str2 ^ " is out of PP!\n\n"); battle_loop btl)
        else 
          (print_string (str2 ^ " is not an available move!\n\n\n"); 
           battle_loop btl)
      | Quit -> print_string "Quitting ...\n\n\n"; exit 0
      | _ -> ANSITerminal.erase Above;
        print_string "Invalid Command - 
                  Type 'Help' if you need help\n\n\n"; 
    end

let rec chess_loop chess_game =
  match read_line () with
  | str -> begin
      match Command.parse_phrase str with
      | exception Empty -> 
        ANSITerminal.erase Above;
        print_string "Please enter a valid command:\n\n\n";
        chess_loop chess_game;
      | Move (cmd1,cmd2) ->
        if Command.check_coordinate cmd1 && Command.check_coordinate cmd2 
        then let old_square = 
               ((Char.escaped cmd1.[0]) |> String.uppercase_ascii, 
                Pervasives.int_of_char cmd1.[1] - 48) in
          let new_square =
            ((Char.escaped cmd2.[0]) |> String.uppercase_ascii, 
             Pervasives.int_of_char cmd2.[1] - 48) in
          let next_move = 
            (try (ChessGame.move old_square new_square chess_game) with
             | InvalidMove -> ANSITerminal.erase Above;
               print_string "Invalid moveee! Please try again.\n\n\n";
               chess_loop chess_game;
               (None, None, None, chess_game)) in
          match next_move with
          | (Some p1, None, None, next_game) -> print_string "Next\n";
            chess_loop next_game;
          | (Some p1, Some p2, Some new_game, _) -> 
            let new_btl = Battle.make_battle (Chess.pokemon_from_piece (Some p1)) 
                (Chess.pokemon_from_piece (Some p2)) in
            printed new_btl;
            battle_loop new_btl;
            chess_loop chess_game;
          | _ ->
            ANSITerminal.erase Above;
            print_string "Invalid move! Please try again.\n\n\n";
            chess_loop chess_game;
        else ANSITerminal.erase Above;
        print_string "Invalid move! Please try again.\n\n\n"; 
        chess_loop chess_game;
      | Quit -> print_string "Quitting ...\n\n\n"; exit 0
      | _ -> ANSITerminal.erase Above;
        print_string "Invalid Command - 
                  Type 'Help' if you need help\n\n\n";
        chess_loop chess_game;
    end


let play_game () = 
  chess_loop new_chess_game
(*printed battle;
  print_string "You entered a battle with a pokemon. Fight to stay alive\n";
  battle_loop battle*)


let main () =
  ANSITerminal.erase Screen;
  play_game ()

(* Execute the game engine. *)
let () = main ()
