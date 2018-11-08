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


let moves_test = [
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

(** start chess tests *)
let test_colors_match
    (name : string)
    (piece1 : game_piece)
    (piece2 : game_piece)
    (expected : bool) =
  name >:: (fun _ -> assert_equal expected (Chess.colors_match piece1 piece2))

let test_is_free
    (name : string)
    (game_board : board)
    (sq : square)
    (expected : bool) = 
  name >:: (fun _ -> assert_equal expected (Chess.is_free game_board sq))

let get_moves_test
    (name : string)
    (piece : game_piece)
    (board : board) 
    (expected : square list) = 
  name >:: (fun _ -> assert_equal expected (Chess.get_moves piece board))


let pokemon_test = [
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

let pawn = (Pawn (Pokemon.get_pawn ()), Black, ("A", 7), false)
let antipawn = (Pawn (Pokemon.get_pawn ()), White, ("A", 2), false)
let rook = (Rook (Pokemon.get_rook ()), Black, ("A", 1), false)
let test_board = empty |> Chess.add_piece antipawn |> Chess.add_piece rook

let chess_test = [
  test_colors_match "colors match" pawn rook true;
  test_colors_match "colors don't match" pawn antipawn false;
  test_is_free "free square" test_board ("A",3) true;
  get_moves_test "test pawn's moves" antipawn test_board [("A",3);("A",4)];
  get_moves_test "test rook's moves" rook test_board [("H",1);("G",1);("F",1);("E",1);
                                                      ("D",1);("C",1);("B",1);("A",2)]
]

let suite = 
  "test suite for midterm project" >::: List.flatten [
    ptype_tests;
    moves_test;
    pokemon_test;
    chess_test
  ]

let _ = run_test_tt_main suite
