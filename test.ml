open OUnit2 
open Battle
open Command
open Pokemon
open Moves
open Ptype
open Chess


(************************ tests for [ptype.ml] ********************************)

let make_get_effective_test
    (name : string) 
    (type1 : Ptype.t) 
    (type2 : Ptype.t) 
    (expected_output : float) = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ptype.get_effective type1 type2))

(*Creating types water, fire, and ground *)
let water = make_type "water" [("water", 0.5);("fire", 2.)]
let fire = make_type "fire" [("fire", 0.5);("water", 0.5)]
let ground = make_type "ground" []

let ptype_tests = [
  make_get_effective_test "Water on Fire" water fire 2.;
  make_get_effective_test "Fire on Water" fire water 0.5;
  make_get_effective_test "Fire on Fire" fire fire 0.5; 
  make_get_effective_test "Standard effectiveness" ground fire 1.0;
]  

(************************ tests for [move.ml] *********************************)

let make_can_use_test
    (name : string) 
    (move : Moves.t) 
    (expected_output : bool) = 
  name >:: (fun _ -> 
      assert_equal expected_output (Moves.can_use move))

let make_get_pp_test
    (name : string) 
    (move : Moves.t) 
    (expected_output : int) = 
  name >:: (fun _ -> 
      assert_equal expected_output (Moves.get_pp move))

let make_dec_pp_test
    (name : string) 
    (move : Moves.t) = 
  let before_pp = Moves.get_pp move in
  Moves.dec_pp move;
  name >:: (fun _ -> 
      assert_equal (Moves.get_pp move) (if before_pp = 0 then 0 
                                        else before_pp - 1))

let make_get_name_test
    (name : string) 
    (move : Moves.t) 
    (expected_output : string) = 
  name >:: (fun _ -> 
      assert_equal expected_output (Moves.get_name move))

let make_get_type_test
    (name : string) 
    (move : Moves.t) 
    (expected_output : Ptype.t) = 
  name >:: (fun _ -> 
      assert_equal expected_output (Moves.get_type move))                

let make_get_description_test
    (name: string) 
    (move: Moves.t) 
    (expected: string) = 
  name >:: (fun _ -> 
      assert_equal expected (Moves.get_description move))

let make_get_power_test
    (name: string) 
    (move: Moves.t) 
    (expected: float) = 
  name >:: (fun _ -> 
      assert_equal expected (Moves.get_power move))

let make_get_acc_test
    (name: string) 
    (move: Moves.t)
    (expected: float)  = 
  name >:: (fun _ -> 
      assert_equal expected (Moves.get_acc move))

let make_is_priority_test
    (name: string) 
    (move: Moves.t)
    (expected: bool)  = 
  name >:: (fun _ -> 
      assert_equal expected (Moves.is_priority move))

(* types used for testing the moves *)
let electric = Ptype.make_type "Electric" 
    [("Water", 2.0); ("Flying", 2.0); ("Electric", 0.5); ("Grass", 0.5)]

let normal = Ptype.make_type "Normal" []

(* moves being tested *)
let thunder_shock = Moves.make_move "Thunder Shock" electric 30 "" 40. 1. 0.1 
    false None

let thunder_wave = Moves.make_move "Thunder Wave" electric 0 
    "Paralyzes opponent." 0. 1. 0.1 false None

let quick_attack = Moves.make_move "Quick Attack" 
    normal 30 "" 0. 1. 0.1 true None

let bogus_move = Moves.make_move "" 
    normal 0 "" 0. 1. 0.1 true None


let moves_tests = [
  make_can_use_test "PP has already ran out" thunder_wave false;
  make_can_use_test "PP has not run out yet" thunder_shock true;

  make_dec_pp_test "decreasing pp of move with nonzero pp" thunder_shock;
  make_dec_pp_test "trying to decrease pp of move with zero pp" thunder_wave;

  make_get_name_test "get nonempty move name" quick_attack "Quick Attack";
  make_get_name_test "get empty move name" bogus_move "";

  make_get_type_test "electric type" thunder_shock electric;
  make_get_description_test "getting description" thunder_wave 
    "Paralyzes opponent.";
  make_get_power_test "getting power" thunder_shock 40.;
  make_get_acc_test "getting accuracy" quick_attack 1.;

  make_is_priority_test "priority move" quick_attack true;
  make_is_priority_test "non-priority move" thunder_shock false;
]

(************************ tests for [pokemon.ml] ******************************)

let test_get_moves
    (name: string) 
    (pokemon: Pokemon.t)
    (expected: Moves.t list) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_moves pokemon))

let test_get_poke_name
    (name: string)
    (pokemon: Pokemon.t)
    (expected: string) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_name pokemon))

let test_get_poke_type
    (name: string)
    (pokemon: Pokemon.t)
    (expected: Ptype.t *Ptype.t option) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_type pokemon))

let test_get_attr
    (name: string)
    (pokemon: Pokemon.t)
    (expected: float list) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_attr pokemon))

let test_get_mult
    (name: string)
    (pokemon: Pokemon.t)
    (expected: float list) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_mult pokemon))

let test_get_curr_hp
    (name: string)
    (pokemon: Pokemon.t)
    (expected: int) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_curr_hp pokemon))

let test_get_max_health
    (name: string)
    (pokemon: Pokemon.t)
    (expected: int) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_max_health pokemon))

let test_get_status
    (name: string)
    (pokemon: Pokemon.t)
    (expected: Moves.status option) =
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_status pokemon))

let test_change_health
    (name: string)
    (pokemon: Pokemon.t)
    (dam: int) =
  let expected = (Pokemon.get_curr_hp pokemon)- dam in
  Pokemon.change_health pokemon (-dam);
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_curr_hp pokemon))

let test_change_attr_mult
    (name: string)
    (pokemon: Pokemon.t)
    (expected: float list) =
  Pokemon.change_attr_mult pokemon expected;
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_mult pokemon))

let test_change_status
    (name: string)
    (pokemon: Pokemon.t)
    (expected: Moves.status option) =
  Pokemon.change_status pokemon expected;
  name >:: (fun _ -> 
      assert_equal expected (Pokemon.get_status pokemon))


(* Pokemon and other types used for testing *)
let grass = Ptype.make_type "Grass" 
    [("Water", 2.0); ("Fire", 0.5); ("Flying", 0.5); ("Grass", 0.5); 
     ("Poison", 0.5)]

let hp_grass = Moves.make_move "Hidden Power Grass" 
    grass 15 "" 60. 1. 0.1 false None

let pikachu = make_pokemon "Pikachu" (electric, None) 
    [thunder_shock; thunder_wave; quick_attack; hp_grass] 
    [180.; 103.; 58.; 166.]

let pokemon_tests = [
  test_get_moves "pika's moves" pikachu 
    [thunder_shock; thunder_wave; quick_attack; hp_grass];
  test_get_poke_name "get poke name" pikachu "Pikachu"; 
  test_get_poke_type "poke_type" pikachu (electric, None);
  test_get_mult "poke_multiplier" pikachu [1.5;1.5;1.;1.];
  test_get_curr_hp "pikachu's health" pikachu 150;
  test_get_max_health "pikachu's max health" pikachu 180;
  test_get_status "pikachu's status" pikachu (Some Poison);
  test_change_health "changing health" pikachu 30;
  test_change_attr_mult "changing mult" pikachu [1.5;1.5;1.;1.];
  test_change_status "changing status" pikachu (Some Poison)
]

(************************ tests for [chess.ml] ******************************)

let make_colors_match_test
    (name : string)
    (piece1 : game_piece)
    (piece2 : game_piece)
    (expected : bool) =
  name >:: (fun _ -> assert_equal expected (Chess.colors_match piece1 piece2))

let make_is_free_test
    (name : string)
    (game_board : board)
    (sq : square)
    (expected : bool) = 
  name >:: (fun _ -> assert_equal expected (Chess.is_free game_board sq))

let make_get_piece_test
    (name : string)
    (board : board)
    (sq : square)
    (expected_output : game_piece option) =
  name >:: (fun _ -> 
      assert_equal expected_output (Chess.get_piece board sq))

let make_can_capture_test
    (name : string)
    (piece1 : game_piece)
    (piece2 : game_piece)
    (expected_output : bool) =
  name >:: (fun _ ->
      assert_equal expected_output (Chess.can_capture piece1 piece2))

let rec same_lst_helper lst1 lst2 =
  match lst1 with
  | [] -> true
  | h :: t -> if not (List.mem h lst2) then false else same_lst_helper t lst2

(** [same_lst lst1 lst2] is whether or not [lst1] and [lst2] contain the same 
    elements, i.e. all elements that are in [lst1] are also in [lst2], and 
    vice versa. *)
let same_lst lst1 lst2 =
  if List.length lst1 <> List.length lst2 then false else
    same_lst_helper lst1 lst2

let make_get_moves_test
    (name : string)
    (piece : game_piece)
    (board : board) 
    (expected_output : square list) = 
  name >:: (fun _ -> assert_bool "different elements" 
               (same_lst expected_output (Chess.get_moves piece board)))

let make_pieces_equal_test
    (name : string)
    (piece1 : game_piece)
    (piece2 : game_piece)
    (expected_output : bool) =
  name >:: (fun _ -> assert_equal expected_output (piece1 = piece2))

let make_current_player_equal_test
    (name : string)
    (color : color)
    (game : ChessGame.t)
    (expected_output : bool) = 
  name >:: (fun _ -> assert_equal expected_output (color = ChessGame.get_current_player game))

let int_of_letter ltr = 
  (String.get ltr 0 |> Char.uppercase_ascii |> Char.code) - 65

let piece_at_square_equals_test
    (name : string)
    ((c,r) : (string * int))
    (game : ChessGame.t)
    (expected_output : string) = 
  name >:: (fun _ -> 
      let col = int_of_letter c in
      let game_list = ChessGame.as_list game in 
      let row = List.nth game_list col in
      let (sq, p, c_opt, c)  = List.nth row (r - 1) in
      let piece_string = (
        match p with
        | None -> "empty"
        | Some (Pawn _) -> "pawn"
        | Some (Rook _) -> "rook"
        | Some (Knight _) -> "knight"
        | Some (Bishop _) -> "bishop"
        | Some (Queen _) -> "queen"
        | Some (King _) -> "king"
        | Some (FakePawn _) -> "error"
      ) in 
      assert_equal expected_output piece_string)

(** [move_game game move_list] moves [game] according to the assoc list of moves,
    [move_list] *)
let rec move_game game  = function 
  | [] -> game
  | (sq1,sq2)::t -> 
    let (_,_,n,l)  = (ChessGame.move sq1 sq2 game) in 
    let ng = (
      match n with 
      | None -> l
      | Some new_game -> new_game
    ) in 
    move_game ng t


let column_type_one : column =     [(1,(Black, None)); (2,(White, None)); 
                                    (3,(Black, None)); (4,(White, None));
                                    (5,(Black, None)); (6,(White, None));
                                    (7,(Black, None)); (8,(White, None))]

let column_type_two : column = [(1,(White, None)); (2,(Black, None));
                                (3,(White, None)); (4,(Black, None)); 
                                (5,(White, None)); (6,(Black, None)); 
                                (7,(White, None)); (8,(Black, None))]
let empty : board = 
  [("A",column_type_one); ("B",column_type_two);
   ("C", column_type_one); ("D",column_type_two); 
   ("E",column_type_one); ("F", column_type_two); 
   ("G", column_type_one); ("H", column_type_two)]

let black_pawn = (Pawn (Pokemon.get_pawn ()), Black, ("A", 7), false)
let white_pawn = (Pawn (Pokemon.get_pawn ()), White, ("A", 2), false)
let black_rook = (Rook (Pokemon.get_rook ()), Black, ("H", 8), false)
let test_board = empty |> Chess.add_piece white_pawn |> 
                 Chess.add_piece black_rook |> Chess.add_piece black_pawn

let white_bishop = (Bishop (Pokemon.get_bishop ()), White, ("C", 1), false)
let black_knight = (Knight (Pokemon.get_knight ()), Black, ("G", 8), false)
let white_queen = (Queen (Pokemon.get_queen ()), White, ("D", 1), false)
let black_king = (King (Pokemon.get_king ()), Black, ("E", 8), false)
let test_board2 = test_board |> Chess.add_piece black_knight |> 
                  Chess.add_piece white_bishop |> Chess.add_piece white_queen
                  |> Chess.add_piece black_king

let black_pawn2 = (Pawn (Pokemon.get_pawn ()), Black, ("B", 3), true)
let black_pawn3 = (Pawn (Pokemon.get_pawn ()), Black, ("F", 4), true)
let black_rook2 = (Rook (Pokemon.get_rook ()), Black, ("F", 1), true)
let white_pawn2 = (Pawn (Pokemon.get_pawn ()), White, ("H", 5), true)
let white_pawn3 = (Pawn (Pokemon.get_pawn ()), White, ("E", 7), true)
let test_board3 = test_board2 |> Chess.add_piece black_pawn2 
                  |> Chess.add_piece white_pawn2 |> Chess.add_piece white_pawn3
                  |> Chess.add_piece black_pawn3 |> Chess.add_piece black_rook2

let wpawn1 = (Pawn (Pokemon.get_pawn ()), White, ("A", 2), true)
let wpawn2 = (Pawn (Pokemon.get_pawn ()), White, ("B", 2), false)
let wpawn3 = (Pawn (Pokemon.get_pawn ()), White, ("C", 2), true)
let wpawn4 = (Pawn (Pokemon.get_pawn ()), White, ("D", 2), true)
let wpawn5 = (Pawn (Pokemon.get_pawn ()), White, ("E", 2), true)
let wpawn6 = (Pawn (Pokemon.get_pawn ()), White, ("F", 2), true)
let wpawn7 = (Pawn (Pokemon.get_pawn ()), White, ("G", 2), true)
let white_rook = (Rook (Pokemon.get_rook ()), White, ("A", 1), false)
let white_knight = (Knight (Pokemon.get_knight ()), White, ("B", 1), false)
let white_bishop2 = (Bishop (Pokemon.get_bishop ()), White, ("F", 1), false)
let white_king = (King (Pokemon.get_king ()), White, ("E", 1), false)
let black_pawn_block = (Pawn (Pokemon.get_pawn ()), Black, ("B", 3), true)
let white_pawn_block = (Pawn (Pokemon.get_pawn ()), White, ("A", 3), true)
let white_rook_block = (Rook (Pokemon.get_rook ()), White, ("C", 3), false)
let test_board4 = 
  empty |> Chess.add_piece wpawn1 |> Chess.add_piece wpawn2 
  |> Chess.add_piece wpawn3 |> Chess.add_piece wpawn4 |> Chess.add_piece wpawn5 
  |> Chess.add_piece wpawn6 |> Chess.add_piece wpawn7 
  |> Chess.add_piece white_king |> Chess.add_piece white_knight 
  |> Chess.add_piece white_queen |> Chess.add_piece white_bishop 
  |> Chess.add_piece white_rook |> Chess.add_piece white_bishop2 
  |> Chess.add_piece black_pawn_block |> Chess.add_piece white_pawn_block 
  |> Chess.add_piece white_rook_block


let precastling_board = empty |> Chess.add_piece white_king
                        |> Chess.add_piece white_rook |> Chess.add_piece wpawn4 
                        |> Chess.add_piece wpawn5 |> Chess.add_piece wpawn6

let postcastling_board = Chess.move white_king precastling_board ("C", 1)
let postcastling_king = 
  match (Chess.get_piece postcastling_board ("C", 1)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")

let postcastling_rook = 
  match (Chess.get_piece postcastling_board ("D", 1)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")


let test_board3_turn1 = Chess.move white_pawn test_board3 ("A", 4)
let postmove_pawn = 
  match (Chess.get_piece test_board3_turn1 ("A", 4)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")
let test_board3_turn2 = Chess.move black_knight test_board3_turn1 ("F", 6)
let postmove_knight = 
  match (Chess.get_piece test_board3_turn2 ("F", 6)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")
let test_board3_turn3 = Chess.move white_bishop test_board3_turn2 ("B", 2)
let postmove_bishop =
  match (Chess.get_piece test_board3_turn3 ("B", 2)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")
let test_board3_turn4 = Chess.move black_rook test_board3_turn3 ("H", 6)
let postmove_rook =
  match (Chess.get_piece test_board3_turn4 ("H", 6)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")
let test_board3_turn5 = Chess.move white_queen test_board3_turn4 ("D", 5)
let postmove_queen =
  match (Chess.get_piece test_board3_turn5 ("D", 5)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")
let test_board3_turn6 = Chess.move black_king test_board3_turn5 ("D", 8)
let postmove_king =
  match (Chess.get_piece test_board3_turn6 ("D", 8)) with
  | Some p -> p
  | None -> raise (Invalid_argument "")



(* [white_pawn_turn2] represents [white_pawn] after it has moved from
    square [("A", 2)] to square [("A", 4)]. *)
let white_pawn_turn2 = (Pawn (Pokemon.get_pawn ()), White, ("A", 4), true)

let new_chess_game = Chess.ChessGame.new_game

let opening = [(("D",2),("D",4));(("E",7),("E",5));(("D",4),("E",5))]

let open_chess_game = move_game new_chess_game opening

let knight_play = [(("B",8),("C",6));(("G",1),("F",3));(("C",6),("E",5));(("F",3),("E",5))]

let knight_game = move_game open_chess_game knight_play

let bishop_play = [(("F",8),("D",6));(("C",1),("F",4));(("D",6),("E",5));(("F",4),("E",5))]

let bishop_game = move_game knight_game bishop_play

let queen_play = [(("D",8),("F",6));(("D",1),("D",7));(("C",8),("D",7));(("E",5),("F",6))]

let no_queens_game = move_game bishop_game queen_play

let castle_queens = [(("G",8),("F",6));(("B",1),("C",3));(("E",8),("G",8));(("E",1),("C",1))]

let castled = move_game no_queens_game castle_queens

let en_passant_play = [(("G",7),("G",5));(("B",2),("B",4));(("G",5),("G",4));
                       (("B",4),("B",5));(("C",7),("C",5));(("B",5),("C",6));
                       (("A",8),("C",8));(("H",2),("H",4));(("G",4),("H",3))]
let en_passant_passes = move_game castled en_passant_play

let upgrade_pieces = [(("H",1),("G",1));(("H",3),("H",2));(("C",6),("C",7));
                      (("H",2),("H",1));(("D",1),("D",7));(("C",8),("A",8));
                      (("C",7),("C",8))]

let promoted_board = move_game en_passant_passes upgrade_pieces

let test_board_turn2 = test_board |> Chess.add_piece white_pawn_turn2

let chess_tests = [
  make_colors_match_test "both pieces black" black_pawn black_rook true;
  make_colors_match_test "both pieces white" white_pawn white_bishop true;
  make_colors_match_test "colors don't match" black_pawn white_pawn false;

  make_is_free_test "free square" test_board ("A",3) true;
  make_is_free_test "occupied square" test_board ("A", 7) false;

  make_get_piece_test "get pawn" test_board ("A", 7) (Some black_pawn);
  make_get_piece_test "empty square" test_board ("B", 8) None;
  "check InvalidSquare exception" >:: (fun _ ->
      assert_raises (InvalidSquare ("X", 2)) (fun () ->
          Chess.get_piece test_board ("X", 2)));

  make_can_capture_test "colors don't match" black_pawn white_bishop true;
  make_can_capture_test "both pieces black" black_pawn black_rook false;
  make_can_capture_test "both pieces white" white_bishop white_pawn false;

  (* testing [get_move] when capturing pieces is not possible *)
  make_get_moves_test "test pawn's moves when it hasn't moved already" 
    white_pawn test_board [("A",3);("A",4)];
  make_get_moves_test "test pawn's moves after it has already moved once" 
    white_pawn_turn2 test_board_turn2 [("A",5)];
  make_get_moves_test "test rook's moves" black_rook test_board 
    [("H", 7);("H", 6);("H", 5);("H", 4);("H", 3);("H", 2);("H", 1);
     ("G", 8); ("F", 8); ("E", 8); ("D", 8); ("C", 8); ("B", 8); ("A", 8)];
  make_get_moves_test "test knight's moves" black_knight test_board2 
    [("H", 6); ("F", 6); ("E", 7)];
  make_get_moves_test "test bishop's moves" white_bishop test_board2 
    [("B", 2); ("A", 3); ("D", 2); ("E", 3); ("F", 4); ("G", 5); ("H", 6)];
  make_get_moves_test "test queen's moves" white_queen test_board2
    [("C", 2); ("B", 3); ("A", 4); ("D", 2); ("D", 3); ("D", 4); ("D", 5); 
     ("D", 6); ("D", 7); ("D", 8); ("E", 2); ("F", 3); ("G", 4); ("H", 5);
     ("E", 1); ("F", 1); ("G", 1); ("H", 1)];
  make_get_moves_test "test king's moves" black_king test_board2 
    [("D", 8); ("F", 8); ("E", 7); ("D", 7); ("F", 7)];

  (* testing [get_move] when capturing pieces is possible *)
  make_get_moves_test "pawn moves when capturing piece is possible" 
    white_pawn test_board3 [("A", 3); ("A", 4); ("B", 3)];
  make_get_moves_test "rook moves when capturing piece is possible" 
    black_rook test_board3 [("H", 7);("H", 6);("H", 5)];
  make_get_moves_test "knight moves when capturing piece is possible" 
    black_knight test_board3 [("H", 6); ("F", 6); ("E", 7)];
  make_get_moves_test "bishop moves when capturing piece is possible" 
    white_bishop test_board3 [("B", 2); ("A", 3); ("D", 2); ("E", 3); ("F", 4)];
  make_get_moves_test "queen moves when capturing piece is possible" 
    white_queen test_board3
    [("C", 2); ("B", 3); ("D", 2); ("D", 3); ("D", 4); ("D", 5); ("D", 6); 
     ("D", 7); ("D", 8); ("E", 2); ("F", 3); ("G", 4); ("E", 1); ("F", 1)];
  make_get_moves_test "king moves when capturing piece is possible" 
    black_king test_board3 [("D", 8); ("F", 8); ("E", 7); ("D", 7); ("F", 7)];

  (* testing [get_move] when there are no possible moves *)
  make_get_moves_test "pawn can't move" wpawn2 test_board4 [];
  make_get_moves_test "rook can't move" white_rook test_board4 [];
  make_get_moves_test "knight can't move" white_knight test_board4 [];
  make_get_moves_test "bishop can't move" white_bishop test_board4 [];
  make_get_moves_test "queen can't move" white_queen test_board4 [];
  make_get_moves_test "king can't move" white_king test_board4 [];

  (* testing castling *)
  make_get_moves_test "test that king can castle" white_king precastling_board 
    [("D", 1); ("C", 1); ("F", 1)];
  make_pieces_equal_test "king is in expected position after castling" 
    (King (Pokemon.get_king ()), White, ("C", 1), true)
    postcastling_king true;
  make_pieces_equal_test "rook is in expected position after castling" 
    (Rook (Pokemon.get_rook ()), White, ("D", 1), true) postcastling_rook true;

  (* testing [move] *)
  make_pieces_equal_test "pawn is in expected position after moving"
    (Pawn (Pokemon.get_pawn ()), White, ("A", 4), true) postmove_pawn true;
  make_pieces_equal_test "knight is in expected position after moving"
    (Knight (Pokemon.get_knight ()), Black, ("F", 6), true) 
    postmove_knight true;
  make_pieces_equal_test "bishop is in expected position after moving"
    (Bishop (Pokemon.get_bishop ()), White, ("B", 2), true) 
    postmove_bishop true;
  make_pieces_equal_test "rook is in expected position after moving"
    (Rook (Pokemon.get_rook ()), Black, ("H", 6), true) postmove_rook true;
  make_pieces_equal_test "queen is in expected position after moving"
    (Queen (Pokemon.get_queen ()), White, ("D", 5), true) postmove_queen true;
  make_pieces_equal_test "king is in expected position after moving"
    (King (Pokemon.get_king ()), Black, ("D", 8), true) postmove_king true;


  (* testing state of ChessGame.t *)
  make_current_player_equal_test "new chess game" White new_chess_game true;

  piece_at_square_equals_test "pawn at e5" ("E",5) open_chess_game "pawn";

  make_current_player_equal_test "pawn game" Black open_chess_game true;

  piece_at_square_equals_test "knight at e5" ("E",5) knight_game "knight";

  piece_at_square_equals_test "bishop at e5" ("E",5) bishop_game "bishop";

  piece_at_square_equals_test "bishop at f6" ("F",6) no_queens_game "bishop"; 

  piece_at_square_equals_test "rook at f8" ("F",8) castled "rook";
  piece_at_square_equals_test "rook at d1" ("D",1) castled "rook";
  piece_at_square_equals_test "king at c1" ("C",1) castled "king";
  piece_at_square_equals_test "king at g8" ("G",8) castled "king";

  piece_at_square_equals_test "pawn en-passant captured at c6" ("C",6) 
    en_passant_passes "pawn";
  piece_at_square_equals_test "pawn en-passant captured at h3" ("H",3)
    en_passant_passes "pawn";

  piece_at_square_equals_test "promoted pawn, so queen, at h1" ("H",1)
    promoted_board "queen";

  piece_at_square_equals_test "promoted pawn, so queen, at c8" ("C",8)
    promoted_board "queen";

  make_current_player_equal_test "promoted board should be black" Black
    promoted_board true

]

let suite = 
  "test suite for midterm project" >::: List.flatten [
    ptype_tests;
    moves_tests;
    pokemon_tests;
    chess_tests
  ]

let _ = run_test_tt_main suite
