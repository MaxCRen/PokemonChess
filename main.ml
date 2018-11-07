open Battle
open Pokemon
open Moves
open Ptype
open Command
open Random
open Chess
open ChessGame

let new_chess_game = ChessGame.new_game
let player_fainted = ref false
let opp_fainted = ref false
let first_square = ref true
let fainted_green_pieces = ref []
let fainted_red_pieces = ref []

let print_colored btl str poke=
  if poke == Battle.get_player btl then 
    ANSITerminal.(print_string [green] str)
  else ANSITerminal.(print_string[red] str)
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
|                                `'                            '-._|    |
_________________________________________________________________________\n\n")

let print_str_color str (color:Chess.color) =
  match color with 
  | White -> ANSITerminal.(print_string [green] str)
  | Black -> ANSITerminal.(print_string [red] str)

(*
let print_nth_of_col 
    (square: Chess.square * Chess.piece option * Chess.color option * Chess.color) 
    (blue_squares : Chess.square list) =
  ANSITerminal.(print_string [yellow]  "#");
  match square with 
  | (sq, None, _, _) ->
    print_string "       "
  | (_, Some (Pawn x), Some c, _) -> print_str_color "   P   " c
  | (_, Some (Rook x), Some c, _) -> print_str_color "   R   " c
  | (_, Some (Knight x), Some c, _) -> print_str_color "   Z   " c
  | (_, Some (Bishop x), Some c, _) -> print_str_color "   B   " c
  | (_, Some (Queen x), Some c, _) -> print_str_color "   Q   " c
  | (_, Some (King x), Some c, _) -> print_str_color "   K   " c
  | _ -> print_string "error, all pieces should have a color"
let print_line_gaps () (r:int)=
  if r = -1 then 
    ANSITerminal.(print_string 
                    [yellow] 
                    ("#\n#       #       #       #       #       #       #       #       #\n"))
  else
    ANSITerminal.(print_string 
                    [yellow] ("#   "^(string_of_int (r+1))^"\n#       #       #       #       #       #       #       #       #\n"))
*)
let print_border_lines () =
  ANSITerminal.(print_string [yellow]  
                  "#################################################################\n")

let print_letters () =
  ANSITerminal.(print_string [yellow]  
                  "\n    A       B       C       D       E       F       G       H\n\n")

(*
let rec print_row 
    (board: 
       ((Chess.square * Chess.piece option * Chess.color option * Chess.color)
          list) list) (r:int)
    (blue_squares : Chess.square list) = 
  match board with 
  | [] -> print_line_gaps () r
  | col::t ->  print_nth_of_col (List.nth col r) blue_squares; 
    print_row t r blue_squares
let rec print_blue_square2 () counter =
  if counter = 0 then ()
  else (ANSITerminal.(print_string [yellow] "#\n#");
        ANSITerminal.(print_string [blue] "*******");
        print_blue_square2 () (counter - 1))
let rec board_helper 
    (board: ((Chess.square * Chess.piece option * 
              Chess.color option * Chess.color) list) list) (r:int)
    (blue_squares : Chess.square list) =
  if r < 0 then () else
    match board with
    | [] -> ()
    | col::_ -> (match List.nth col r with
        | (sq, _, _, _) -> if List.mem sq blue_squares 
          then print_blue_square2 () 3
          else print_line_gaps () (-1));
      print_row board r blue_squares; 
      print_border_lines (); board_helper board (r-1) blue_squares*)

let print_normal_square (square: Chess.square * Chess.piece option 
                                 * Chess.color option * Chess.color) =
  ANSITerminal.(print_string [yellow]  "#");
  match square with 
  | (sq, None, _, _) -> print_string "       "
  | (_, Some (Pawn x), Some c, _) -> print_str_color "   P   " c
  | (_, Some (Rook x), Some c, _) -> print_str_color "   R   " c
  | (_, Some (Knight x), Some c, _) -> print_str_color "   Z   " c
  | (_, Some (Bishop x), Some c, _) -> print_str_color "   B   " c
  | (_, Some (Queen x), Some c, _) -> print_str_color "   Q   " c
  | (_, Some (King x), Some c, _) -> print_str_color "   K   " c
  | _ -> print_string "error, all pieces should have a color"

let print_blue_square (square: Chess.square * Chess.piece option 
                               * Chess.color option * Chess.color) =
  ANSITerminal.(print_string [yellow]  "#");
  match square with
  | (_, None, _, _) -> ANSITerminal.(print_string [blue] "*******");
  | (_, Some (Pawn x), Some c, _) -> ANSITerminal.(print_string [blue] "***");
    print_str_color "P" c;
    ANSITerminal.(print_string [blue] "***")
  | (_, Some (Rook x), Some c, _) -> ANSITerminal.(print_string [blue] "***");
    print_str_color "R" c;
    ANSITerminal.(print_string [blue] "***")
  | (_, Some (Knight x), Some c, _) -> ANSITerminal.(print_string [blue] "***");
    print_str_color "Z" c;
    ANSITerminal.(print_string [blue] "***")
  | (_, Some (Bishop x), Some c, _) -> ANSITerminal.(print_string [blue] "***");
    print_str_color "B" c;
    ANSITerminal.(print_string [blue] "***")
  | (_, Some (Queen x), Some c, _) -> ANSITerminal.(print_string [blue] "***");
    print_str_color "Q" c;
    ANSITerminal.(print_string [blue] "***")
  | (_, Some (King x), Some c, _) -> ANSITerminal.(print_string [blue] "***");
    print_str_color "K" c;
    ANSITerminal.(print_string [blue] "***")
  | _ -> print_string "error, all pieces should have a color"

let rec print_row (board: ((Chess.square * Chess.piece option 
                            * Chess.color option * Chess.color)list) list) 
    (r:int) (subrow:int) (blue_squares : Chess.square list) 
    (print_blue : bool) = 
  match board with
  | [] -> ()
  | col :: t -> let curr_sq = List.nth col r in
    (match curr_sq with
     | (sq, _, _, _) -> if print_blue && List.mem sq blue_squares 
       then (match subrow with
           | 1 | 3 -> ANSITerminal.(print_string [yellow] "#"); 
             ANSITerminal.(print_string [blue] "*******");
           | 2 -> print_blue_square curr_sq;
           | _ -> ())
       else (match subrow with
           | 1 | 3 -> ANSITerminal.(print_string [yellow] "#       ");
           | 2 -> print_normal_square curr_sq
           | _ -> ()));
    print_row t r subrow blue_squares print_blue

let rec print_board_helper (board: ((Chess.square * Chess.piece option * 
                                     Chess.color option * Chess.color) list) 
                                list) (r:int)
    (blue_squares : Chess.square list) (print_blue : bool) =
  if r < 0 then () else (
    print_row board r 1 blue_squares print_blue;
    ANSITerminal.(print_string [yellow] "#\n");
    print_row board r 2 blue_squares print_blue;
    ANSITerminal.(print_string [yellow] ("#   " ^ (string_of_int (r+1))^ "\n"));
    print_row board r 3 blue_squares print_blue;
    ANSITerminal.(print_string [yellow] "#\n");
    print_border_lines ();
    print_board_helper board (r - 1) blue_squares print_blue)

(*
let rec board_helper 
    (board: ((Chess.square * Chess.piece option * 
              Chess.color option * Chess.color) list) list) (r:int)
    (blue_squares : Chess.square list) =
  if r < 0 then () else
    match board with
    | [] -> ()
    | col::_ -> (match List.nth col r with
        | (sq, _, _, _) -> if List.mem sq blue_squares 
          then print_blue_square2 () 3
          else print_line_gaps () (-1));
      print_row board r blue_squares; 
      print_border_lines (); board_helper board (r-1) blue_squares*)

let rec print_fainted_pieces piece_lst color =
  match piece_lst with
  | [] -> ANSITerminal.(print_string [red] "\n")
  | h :: t -> (match h with
      | Pawn _ -> ANSITerminal.(print_string [color] "P   ")
      | Rook _ -> ANSITerminal.(print_string [color] "R   ")
      | Knight _ -> ANSITerminal.(print_string [color] "Z   ")
      | Bishop _ -> ANSITerminal.(print_string [color] "B   ")
      | Queen _ -> ANSITerminal.(print_string [color] "Q   ")
      | King _ -> ANSITerminal.(print_string [color] "K   ")
      | _ -> ANSITerminal.(print_string [color] ""));
    print_fainted_pieces t color


let print_board (board: ((Chess.square * Chess.piece option * 
                          Chess.color option * Chess.color) list) list) 
    (blue_squares : Chess.square list) (print_blue : bool) =
  print_logo ();
  ANSITerminal.(print_string [red] "Fainted pieces: ");
  print_fainted_pieces (!fainted_red_pieces) ANSITerminal.red;
  print_border_lines ();
  print_board_helper board 7 blue_squares print_blue;
  ANSITerminal.(print_string [green] "Fainted pieces: ");
  print_fainted_pieces (!fainted_green_pieces) ANSITerminal.green;
  print_letters ()



(** [get_move_names moves] is a list of the names of all moves in [moves], with
    each name in all lowercase. *)
let get_move_names moves =
  List.map (fun x -> Moves.get_name x) moves

(* Some Tests *)
let rand = Random.self_init

let water = make_type "water" [("water", 0.5);("fire", 2.)]

let fire = make_type "fire" [("fire", 0.5);("water", 0.5)]

let bubble = make_move ("Bubble") (water) (25) 
    ("Does damage") (10.) (1.) (0.5) false (Some (Heal(-0.1, true)))

let random2 = make_move ("Sleep") (water) (25) 
    ("Does damage") (0.) (1.) (0.) false (Some (Condition (Sleep (5), 1.)))

let inc_stats = make_move ("incr") (water) (25) ("stuff") (0.)
    (1.) (0.) false (Some (Stats ([0.; 0.5; 0.5; 0.5], true)))

let poison = make_move ("Poison") (water) (25) 
    ("Poisons the pokemon") (0.) (1.) (0.) false (Some (Condition (Burned , 1.)))

let ember = make_move ("Ember") (fire) (25) 
    ("Does damage") (1.) (1.) (0.10) false None

(** [get_move_names moves] is a list of the names of all moves in [moves], with
    each name in all lowercase. *)
let get_move_names moves =
  List.map (fun x -> Moves.get_name x |> String.lowercase_ascii) moves

(* placeholder pokemon until we implement battles in full *)
let squirtle = Pokemon.make_pokemon "Squirtle" (water, None) [bubble; random2] 
    [44.;98.;129.;56.] 

let charmander = Pokemon.make_pokemon "Charmander" (fire, None) [ember;ember;ember;ember]
    [39.;112.;93.;55.] 


let battle = Battle.make_battle squirtle charmander 


let print_eff move poke = 
  if Moves.get_power move = 0. then () else(
    match Battle.calc_effective move poke with
    | x -> if x >= 2. then print_string "It's super effective!\n\n\n"
      else if x < 0. then print_string "It's not very effective...\n\n\n"
      else if x = 0. then print_string "But nothing happened...\n\n\n")

(** [hp_display number pokemon] returns a rough percentage of [number] '*'
    characters that is equivalent to (current_health / max_health) of 
    pokemon [poke] *)
let hp_display num poke = 
  let percentage_health = 
    (float_of_int (get_curr_hp poke)) /. (float_of_int (get_max_health poke)) in
  String.make (int_of_float (percentage_health *. num)) '*'

let get_attr int = 
  match int with 
  |1 -> "ATK x"
  |2 -> "DEF x"
  |3 -> "SPD x"
  |_ -> ""

let rec string_of_list = function
  |[] -> "]"
  |h::t -> (h|> Pervasives.string_of_float)^";"^(string_of_list t)

let display_mults bat poke =
  let rec display' bat attr_mult poke count =
    match attr_mult with
    |[] -> ()
    | 1.::t -> display' bat t poke (count+1) 
    | h::t -> let multiplier = Pervasives.string_of_float h in
      let attribute = get_attr count in
      print_colored bat ("["^attribute^multiplier^"]") poke;
      display' bat t poke (count+1) in
  display' bat (Pokemon.get_mult poke) poke 0

(** display_moves[moves] displays all of the moves available to the player's
    pokemon *)
let rec display_moves btl pokemon moves =
  match moves with
  | [] -> print_string("\n")
  | h::t when ((List.length t) mod 2 = 0) -> (
      print_colored btl ("\t\t\t\t["^(Moves.get_name h)^": "^(h |> Moves.get_pp |> Pervasives.string_of_int)^"pp]") pokemon; 
      display_moves btl pokemon t)
  | h::t ->  print_colored btl ("\n\n["^(Moves.get_name h)^": "^(h |> Moves.get_pp |> Pervasives.string_of_int)^"pp]") pokemon; 
    (display_moves btl pokemon t)

let display_status poke = 
  match (Pokemon.get_status poke) with
  | Some Poison -> ANSITerminal.(print_string[blue] "[PSN]")
  | Some Paralyzed _ -> ANSITerminal.(print_string[yellow] "[PAR]")
  | Some Sleep _-> ANSITerminal.(print_string[yellow] "[SLP]")
  | Some Burned -> ANSITerminal.(print_string[magenta] "[BUR]")
  | Some Frozen _-> ANSITerminal.(print_string[cyan] "[FRZ]")
  | _ -> ()

let print_poke_stats btl opponent = 
  display_status opponent;
  ANSITerminal.(print_colored btl (Pokemon.get_name opponent ^ "\n\t\t\tHealth:" ^ 
                                   (hp_display 35. opponent) ^ "\n\t\t\t") opponent);
  let opp_health = get_curr_hp opponent |> Pervasives.string_of_int in
  let opp_max_health = get_max_health opponent |> Pervasives.string_of_int in
  ANSITerminal.(print_colored btl (opp_health^"/"^opp_max_health^" hp\t") opponent);
  display_mults btl opponent

let printed btl = 
  print_string ("_______________________________________________________________________\n");
  let opponent = Battle.get_opponent btl in
  let player = Battle.get_player btl in
  ANSITerminal.(print_colored btl ("\nFIRE RED: \t\t") opponent); 
  print_poke_stats btl opponent;
  print_string "\n\n";
  ANSITerminal.(print_colored btl ("\nLEAF GREEN: \t\t") player); 
  print_poke_stats btl player; 
  print_string "\n";

  print_string("_______________________________________________________________________\n")

(** [printed btl] displays information about the battle [btl] such as the
    current Pokemon on either side, the current HP of both Pokemon,
    and the moveset available to each Pokemon. *)
let printed_leaf btl = 
  let player = Battle.get_player btl in
  printed btl;
  ANSITerminal.(print_string [green] "\t\t\t   Moves");
  Pokemon.get_moves player |> display_moves btl player;
  print_string "\n\n"

let printed_fire btl = 
  print_string ("_______________________________________________________________________\n");
  let opponent = Battle.get_opponent btl in
  let player = Battle.get_player btl in
  ANSITerminal.(print_colored btl ("\nLEAF GREEN: \t\t") player); 
  print_poke_stats btl player;
  print_string "\n\n";
  ANSITerminal.(print_colored btl ("\nFIRE RED: \t\t") opponent); 
  print_poke_stats btl opponent; 
  print_string "\n";
  print_string("_______________________________________________________________________\n");
  ANSITerminal.(print_string [red] "\t\t\t   Moves");
  Pokemon.get_moves opponent |> display_moves btl opponent;
  print_string "\n"

let pause_bet_states battle func (move: Moves.t option) =
  if not (!player_fainted || !opp_fainted) then
    (print_string "Enter anything to continue";
     match move with
     | Some m -> (
         match read_line () with
         |_ ->  (ANSITerminal.erase Screen; func m battle))
     | None ->(
         match read_line () with
         | _ -> ANSITerminal.erase Screen; printed battle))
  else (print_string "Enter anything to continue";
        match read_line () with
        | _ -> ())

(**[check_fainted bat] checks the battle for if either of the pokemon has fainted
   and then performs the correct action accordingly.*)
let check_fainted btl = 
  let opp_poke = Battle.get_opponent btl in
  let curr_poke = Battle.get_player btl in
  if Pokemon.get_curr_hp (Battle.get_opponent btl) <= 0 then 
    (print_string("The opposing " ^ (Pokemon.get_name opp_poke) 
                  ^ " has fainted!\n\n\n"); opp_fainted:=true)
  else if Pokemon.get_curr_hp (Battle.get_player btl) <= 0 then
    (print_string ("Your " ^ (Pokemon.get_name curr_poke) 
                   ^ " has fainted!\n\n\n"); player_fainted:=true)
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
let conditions_helper bat poke status =
  let max_health = Pokemon.get_max_health poke |> Pervasives.float_of_int in
  (match status with 
   | Poison ->  Battle.deal_damage (Pervasives.int_of_float (0.06*.max_health)) poke;
     ANSITerminal.erase Screen;
     printed bat;
     print_colored bat ((Pokemon.get_name poke)^" was hurt by the poison\n") poke;
   | Paralyzed x-> if x - 1 = 0 
     then (Pokemon.change_status poke None; ANSITerminal.erase Screen; 
           printed bat; print_colored bat ((Pokemon.get_name poke)^"'s Paralysis wore off\n") poke)
     else (Pokemon.change_status poke (Some (Paralyzed (x-1)));
           print_colored bat ((Pokemon.get_name poke)^" is still affected by paralysis\n") poke)
   | Sleep x -> if x - 1 = 0 then 
       (print_colored bat ((Pokemon.get_name poke)^" woke up!\n") poke;
        Pokemon.change_status poke None)
     else (Pokemon.change_status poke (Some (Sleep (x-1)));
           print_colored bat ((Pokemon.get_name poke)^" is fast asleep\n") poke)
   | Burned ->  Battle.deal_damage (Pervasives.int_of_float (0.05*.max_health)) poke;
     ANSITerminal.erase Screen;
     printed bat;
     print_colored bat ((Pokemon.get_name poke)^" was hurt by the burn\n") poke;
   | Frozen x -> failwith "Coming soon");
  (* I just want to pause between states, I only pass a function when I'm moving
     a move but we are only displaying what the effect of the status *)
  pause_bet_states bat (fun x y -> ()) None

(**[deal_with_conditions bat] deals with the pokemon's conditions after every
   turn*)
let deal_with_conditions bat =
  let player = Battle.get_player bat in
  let opponent = Battle.get_opponent bat in
  match (Pokemon.get_status player), (Pokemon.get_status opponent) with
  | None, None -> ()
  | Some stat, None -> conditions_helper bat player stat
  | None, Some stat -> conditions_helper bat opponent stat
  | Some stat1, Some stat2 -> conditions_helper bat player stat1; 
    conditions_helper bat opponent stat2


let print_side_eff poke move op_poke= 
  let poke_name = Pokemon.get_name poke in
  let op_name = Pokemon.get_name op_poke in
  match Moves.get_eff move with
  |None -> ()
  |Some x -> begin
      match x with
      |Heal (perc, _) when perc < 0. -> print_string (poke_name^" took recoil!\n")
      |Heal (perc, _) when perc >= 0.-> print_string (poke_name^" Healed!\n")
      |Condition (Poison,_) -> print_string (op_name^" was Poisoned!\n")
      |Condition (Burned, _) when Pokemon.get_status poke <> None -> print_string (op_name^" was Burned!\n")
      |Condition (Sleep _, _) -> print_string (op_name^" fell asleep!\n")
      |Condition (Paralyzed _, _) when Pokemon.get_status poke <> None-> print_string (op_name^" was paralyzed!\n")
      |_ ->()
    end

(**[chances_of float] is true [float]% of the time, and false otherwise*)
let chances_of float =
  let ran_float = Random.float 1. in
  if ran_float <= float then true else false

let use_move_helper btl move poke_name =
  ANSITerminal.erase Screen;
  let crit = Moves.get_crit move in
  if chances_of crit then(
    Battle.use_move btl move true;
    printed btl;
    print_colored btl (poke_name^" used "^(Moves.get_name move)^"\n") (Battle.get_turn btl);
    print_colored btl ("It was a critical hit!\n") (Battle.get_turn btl);
    print_eff move (Battle.other_player btl); check_fainted btl;
    btl |> Battle.other_player |> Battle.change_turn btl)
  else (
    Battle.use_move btl move false;
    printed btl;
    print_colored btl (poke_name^" used "^(Moves.get_name move)^"\n") (Battle.get_turn btl);
    print_eff move (Battle.other_player btl); check_fainted btl;
    btl |> Battle.other_player |> Battle.change_turn btl)

(**[use_move str bat] takes the string representation of a move [str] and then
   applies it to the battle [bat] by either doing damage or some other effect*)
let use_move move btl = 
  if Battle.can_move move then (
    let pokemon = btl |> Battle.get_turn in
    let poke_name = pokemon |> Pokemon.get_name in
    let op_pokemon = Battle.other_player btl in
    let acc_perc = Moves.get_acc move in 


    if (Pokemon.get_status op_pokemon) = None then
      (match Pokemon.get_status pokemon with
       | Some Paralyzed x-> if chances_of 0.25 then (ANSITerminal.erase Screen;
                                                     printed btl;
                                                     print_colored btl (poke_name^" is paralyzed and can not move!\n") pokemon;
                                                     btl |> Battle.other_player |> Battle.change_turn btl)
         else use_move_helper btl move poke_name
       | Some Sleep x-> (ANSITerminal.erase Screen;
                         printed btl;
                         print_colored btl (poke_name^" is asleep and can't move!\n") pokemon;
                         btl |> Battle.other_player |> Battle.change_turn btl)
       |_ when chances_of (1.-.acc_perc) -> (ANSITerminal.erase Screen;
                                             printed btl;
                                             print_colored btl (poke_name^" missed!\n") pokemon;
                                             btl |> Battle.other_player |> Battle.change_turn btl)
       | _ -> use_move_helper btl move poke_name; print_side_eff pokemon move op_pokemon)
    else if (Moves.get_power move = 0. && Moves.has_condition move) 
    then 
      (printed btl; print_colored btl (poke_name^" used "^(Moves.get_name) move^"\n") (Battle.get_turn btl);
       print_string "It failed.\n\n"; Moves.dec_pp move;btl |> Battle.other_player |> Battle.change_turn btl)
    else use_move_helper btl move poke_name


  )
  else (printed btl; print_string "Cannot use move\n\n\n\n";btl |> Battle.other_player |> Battle.change_turn btl)


let goes_first player opponent pl_move op_move = 
  if(Moves.is_priority op_move && Moves.is_priority pl_move) then
    (compare_speed player opponent)
  else 
    (if (Moves.is_priority op_move) then opponent 
     else if (Moves.is_priority pl_move) then player 
     else (compare_speed player opponent))

(*Likely refactor these two methods later, but for now Leave as is.*)
(**[move_first bat str] takes in the battle and the move of the player [str] and
   peforms the move of the pokemon that moves first*)
let move_turns btl str1 str2= 
  let move_list = btl |> Battle.get_player |> Pokemon.get_moves in
  let move = get_move_from_str move_list str1 in
  let player = Battle.get_player btl in
  let opponent = Battle.get_opponent btl in
  let opponent_moves = Pokemon.get_moves opponent in
  let op_move = get_move_from_str opponent_moves str2 in 


  let faster = goes_first player opponent move op_move in
  change_turn btl (faster); 
  if faster == Battle.get_player btl then
    (use_move move btl; pause_bet_states btl use_move (Some op_move);
     if not (!player_fainted || !opp_fainted) then
       pause_bet_states btl use_move None
     else ())
  else (
    use_move op_move btl; pause_bet_states btl use_move (Some move);
    if not (!player_fainted || !opp_fainted) then
      pause_bet_states btl use_move None else ())


let rec get_move btl pokemon= 
  let available_moves = get_move_names (Pokemon.get_moves pokemon) in

  match read_line () with
  | str -> begin
      match Command.parse_phrase_battle str with
      (* Empty then we just deal with a command *)
      | exception Empty -> 
        ANSITerminal.erase Above;
        print_string "Please enter a valid command\n\n\n"; get_move btl pokemon
      | Help -> print_help (); get_move btl pokemon
      | Incorrect ->  
        print_string "Invalid Command - 
                   Type 'Help' if you need help\n\n\n"; 
        get_move btl pokemon
      | Use str2 -> 
        let lc_str = String.lowercase_ascii str2 in
        if List.mem lc_str available_moves then
          let move_list = pokemon |> Pokemon.get_moves in
          let move = Battle.get_move_from_str move_list lc_str in
          if Battle.can_move move then (lc_str)
          else(
            print_string (str2 ^ " is out of PP!\n\n"); get_move btl pokemon)
        else 
          (print_string (str2 ^ " is not an available move!\n\n\n"); 
           get_move btl pokemon)
      | Quit -> print_string "Quitting ...\n\n\n"; exit 0
    end

(* [pause_bool] is used to check if we are in the enter anything to continue state
   or if we are in parsing the player's move state. last_move is the last move
   used by the player so we can use it if the player moves second *)

let rec battle_loop btl = 
  ANSITerminal.erase Screen;
  printed_leaf btl;
  ANSITerminal.(print_string [green] "Leaf Green Move: ";);
  let first_move = btl |> Battle.get_player |> get_move btl in
  ANSITerminal.erase Screen;
  printed_fire btl;
  ANSITerminal.(print_string [red] "Fire Red Move: ";);
  let second_move = btl |> Battle.get_opponent |> get_move btl in
  move_turns btl first_move second_move;
  if not (!player_fainted || !opp_fainted) then(
    deal_with_conditions btl;
    battle_loop btl)
  else if (!player_fainted) then Battle.get_opponent btl
  else Battle.get_player btl



(*[start_battle battle] starts the battle [battle] and plays through until one
  pokemon faints. (basically this was our play game() before we added the chess 
  functionanility, and we call this when one piece tries to caputer another*)
let start_battle battle =
  let opp_poke = Battle.get_opponent battle |> Pokemon.get_name in
  (*printed battle;*)
  print_string ("A wild " ^ opp_poke ^
                " has appeared! Fight to stay alive! \n");
  battle_loop battle

let print_turn chess_game =
  match ChessGame.get_current_player chess_game with
  | White -> ANSITerminal.(print_string[green] "Leaf Green: ");
  | Black -> ANSITerminal.(print_string[red] "Fire Red: ")

let create_new_battle p1 p2 chess_game =
  if ChessGame.get_current_player chess_game = White then(
    let new_btl = 
      Battle.make_battle (Chess.pokemon_from_piece (Some p1)) 
        (Chess.pokemon_from_piece (Some p2)) in
    start_battle new_btl)
  else
    let new_btl = 
      Battle.make_battle (Chess.pokemon_from_piece (Some p2)) 
        (Chess.pokemon_from_piece (Some p1)) in
    start_battle new_btl

let rec print_attributes_helper curr_hp attr color counter =
  match attr with
  | [] -> ANSITerminal.(print_string [color] ("\n\n"))
  | h :: t -> let attr_name = (match counter with
      | 1 -> "Current HP: "
      | 2 -> "Attack: "
      | 3 -> "Defense: "
      | 4 -> "Speed: "
      | _ -> "") in
    (if (counter = 1) then 
       ANSITerminal.(print_string [color] 
                       (attr_name ^ Pervasives.string_of_int curr_hp ^ "\n"))
     else
       ANSITerminal.(print_string [color] 
                       (attr_name ^ Pervasives.string_of_float h ^ "\n")););
    print_attributes_helper curr_hp t color (counter + 1)

(** [print_attributes chess_game curr_square] prints the attributes (such as 
    current HP, Attack, Defense, and Speed stats) of the Pokemon at
    [curr_square]. *)
let print_attributes chess_game curr_square =
  let text_color = (match ChessGame.get_current_player chess_game with
      | White -> ANSITerminal.green
      | Black -> ANSITerminal.red) in
  let curr_poke = ChessGame.get_poke chess_game curr_square in
  ANSITerminal.(print_string [text_color] 
                  ("Selected Pokemon: " ^ Pokemon.get_name curr_poke 
                   ^ "\n\nATTRIBUTES:\n"));
  print_attributes_helper (Pokemon.get_curr_hp curr_poke) 
    (Pokemon.get_attr curr_poke) text_color 1


(** [chess_loop chess_game curr_square blue_squares] controls the main loop
    of the [chess_game] by switching turns, moving pieces, highlighting 
    the squares in [blue_squares] to show where a selected piece is permitted
    to move, etc. *)
let rec chess_loop chess_game curr_square blue_squares =
  player_fainted:=false;
  opp_fainted:=false;
  print_board (ChessGame.as_list chess_game) blue_squares (not(!first_square));
  if (not(!first_square)) then print_attributes chess_game curr_square else ();
  print_turn chess_game;

  match read_line () with
  | str -> begin
      match Command.parse_phrase_chess str with
      | exception Empty -> 
        print_string "Please enter a valid command:\n\n\n";
        chess_loop chess_game curr_square blue_squares;
      | Square (sq) ->
        if (!first_square) then (
          let sq_pair = Chess.get_sq_pair sq in
          let potential_squares = 
            (try (ChessGame.get_moves chess_game sq_pair) with
             | InvalidSquare _ -> []) in
          if (Command.check_coordinate sq 
              && ChessGame.is_player_square chess_game sq_pair
              && List.length (potential_squares) <> 0) then (
            first_square:= not(!first_square);
            chess_loop chess_game sq_pair potential_squares)
          else (
            print_string "Invalid square! Please try again.\n\n\n"; 
            chess_loop chess_game curr_square blue_squares;))
        else (let new_square =
                Chess.get_sq_pair sq in
              let next_move = 
                (try (ChessGame.move curr_square new_square chess_game) with
                 | InvalidMove -> 
                   print_string "Invalid move! Please try again.\n\n\n";
                   chess_loop chess_game curr_square blue_squares;
                   (None, None, None, chess_game)) in
              match next_move with
              | (Some p1, None, None, next_game) -> print_string "";
                first_square:= not(!first_square);
                chess_loop next_game curr_square blue_squares;
              | (Some p1, Some p2, Some new_game, loss_game) -> 
                let player_color = (match ChessGame.get_current_player chess_game with
                    | White -> ANSITerminal.green
                    | Black -> ANSITerminal.red) in
                let survive = create_new_battle p1 p2 chess_game in
                (if (survive == (Chess.pokemon_from_piece (Some p2))) then
                   if (Chess.pokemon_from_piece (Some p1) |> Pokemon.get_name = "Mew") then
                     ((print_endline ((if (ChessGame.get_current_player chess_game) = White then "Green" else "Red") ^ " has lost the game!")); 
                      exit 0;)
                   else (
                     if (player_color = ANSITerminal.green) then 
                       fainted_green_pieces := p1 ::(!fainted_green_pieces) else
                       fainted_red_pieces := p1 ::(!fainted_red_pieces);
                     (first_square:= not(!first_square);
                      chess_loop loss_game curr_square blue_squares))
                 else 
                 if (Chess.pokemon_from_piece (Some p2) |> Pokemon.get_name = "Mew") then
                   ((print_endline ((if (ChessGame.get_current_player chess_game) = White then "Red" else "Green") ^ " has lost game!")); 
                    exit 0;)
                 else (
                   if (player_color = ANSITerminal.green) then 
                     fainted_red_pieces := p2::(!fainted_red_pieces) else
                     fainted_green_pieces := p2::(!fainted_green_pieces);
                   (first_square:= not(!first_square);
                    chess_loop new_game curr_square blue_squares)
                 ))
              | _ ->
                print_string "Invalid move! Please try again.\n\n\n";
                chess_loop chess_game curr_square blue_squares;)
      | Cancel -> 
        if (!first_square) then 
          (print_string "Invalid command! Please try again.\n\n\n";
           chess_loop chess_game curr_square blue_squares)
        else (
          print_string "Move cancelled! Please select new starting square:\n\n";
          first_square:= not(!first_square);
          chess_loop chess_game curr_square blue_squares)
      | Quit -> print_string "Quitting ...\n\n\n"; exit 0
      | _ -> 
        print_string "Invalid Command - 
                  Type 'Help' if you need help\n\n\n";
        chess_loop chess_game curr_square blue_squares;
    end

let play_game () = 
  chess_loop new_chess_game ("A", 0) []

let main () =
  ANSITerminal.erase Screen;
  play_game ()
(* start_battle battle *)

(* Execute the game engine. *)
let () = main ()
