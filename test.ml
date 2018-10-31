open OUnit2 
open Battle
open Command
open Pokemon
open Moves
open Ptype


(************************ PType test Functions ********************************)

(*Function that creates tests for get_effectives*)           
let ptype_get_effectives
(name: string) 
(type1: Ptype.t) 
(type2: Ptype.t) 
(expected: float) = 
  name >:: (fun _ -> 
                assert_equal expected (Ptype.get_effective type1 type2))

(*Testing on types water and fire*)
let water = make_type "water" [("water", 0.5);("fire", 2.)]
let fire = make_type "fire" [("fire", 0.5);("water", 0.5)]

let ptype_tests = [
  ptype_get_effectives "Water on Fire" water fire 2.;
  ptype_get_effectives "Fire on Water" fire water 0.5;
  ptype_get_effectives "Fire on Fire" fire fire 0.5 
]  


let pokemon_tests = []

let suite = 
  "test suite for midterm project" >::: List.flatten [
    pokemon_tests
  ]

let _ = run_test_tt_main suite
