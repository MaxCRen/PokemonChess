open Pokemon
(*We will use the fileRead.ml in order to read from the Pokemon.txt file which 
have pokemon attributes at each line. The goal of this to have the function 
get_poke in which we can put in a line number and then get a pokemon object 
(without moves we will do this seperately). This way, when instantiating the battle
we can just choose a random number and then get the pokemon at the line number from
the txt file.*)

let pokemon = "pokemon.txt"
let moves = "moves.txt"
let types = "types.txt"
let dir = "pokemonStats"

(*[get_absolute_path file_name] is the absolute path of the file file_name*)
let get_absolute_path file_name =
  dir^Filename.dir_sep^file_name 

(* [break_string str] breaks up the line [str] by commas, and is the list of strings
between each of the commas.*)
let break_string str = 
  str |> String.split_on_char ','

(*[filter_string str_lst] is the pokemon.t representation of a pokemon from the string
list str_lst*)
let filter_string str_lst = 
  let poke_lst = break_string str in
  {
    name = poke_lst
  }

