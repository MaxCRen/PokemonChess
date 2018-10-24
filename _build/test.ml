open OUnit2 
open Battle
open Command
open Pokemon
open Moves
open Ptype
open FileRead


let water = Ptype.makeType "water" [("water", 0.5);("fire", 2.)]
let fire = Ptype.makeType "fire" [("fire", 0.5);("water", 2.)]

let bubble = Moves.make_move ("bubble") (water) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])

let ember = Moves.make_move ("ember") (fire) (25) 
                              ("doesdamage") (30.) (1.) (0.10) ([])


let squritle = Pokemon.make_pokemon "Squirtle" (water, None) [bubble] 
                                        [22.;30.;40.;10.] (None)

let charmander = Pokemon.make_pokemon "Charmander" (fire, None) [ember]
                                        [22.;30.;40.;10.] (None)


let pokemon_tests = []

let suite = 
  "test suite for midterm project" >::: List.flatten [
    pokemon_tests
  ]

let _ = run_test_tt_main suite
