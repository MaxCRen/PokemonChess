open Pokemon
open Ptype

(*We will use the fileRead.ml in order to read from the Pokemon.txt file which 
have pokemon attributes at each line. The goal of this to have the function 
get_poke in which we can put in a line number and then get a pokemon object 
(without moves we will do this seperately). This way, when instantiating the battle
we can just choose a random number and then get the pokemon at the line number from
the txt file.*)

exception MissingVal
let pokemon = "Pokemon.txt"
let moves = "Moves.txt"
let types = "Type.txt"
let dir = "pokemonStats"

(*[get_absolute_path file_name] is the absolute path of the file file_name*)
let get_absolute_path file_name =
  dir^Filename.dir_sep^file_name 


(******************Methods for Parsing Pokemon Types**************************)

(*[name_to_eff str] is a list of strings of [str] seperated by a colon. We use
this to seperate the String name of they type with the string representation of
our effectiveness dictionary*)
let name_to_eff str =
  str |> String.split_on_char ':'

(*[eff_val str] is the list of effective types and multiplier values associated.
We assume that the string has the type and the multiplier value directly after*)
let eff_val str =
  str |> String.split_on_char ','

(*[get_eff_dict acc eff_lst] We require that eff_lst is even and that the type
is the first value, and the multiplier is the value directly after. Thus we create
[acc] our (string*int) list that maps Types to their effective value*)
let rec get_eff_dict acc = function 
  |[] -> acc
  |typ::num::t -> get_eff_dict ((typ,(float_of_string num))::acc) t
  |_ -> raise MissingVal  (*Should not be raised because for every type, 
                                        there should be an associated value *)

(*[make_type str] Assuming that str is of the correct for, we give the pokemon
type that the string [str] represents.*)
let make_type str: Ptype.t =
  let temp = str |> name_to_eff in
  List.nth temp 2 |> eff_val |> get_eff_dict [] |> Ptype.makeType (List.hd temp) 

(*[loop_lines str file_channel] is the Ptype.t for the type represented by the
string str. If str is not a valid type, the MissingVal is raised. [file_channel]
is the in_channel for file Type.txt*)
let rec loop_lines str file_channel =
  match Pervasives.input_line file_channel with
  | exception End_of_file -> Pervasives.close_in file_channel; raise MissingVal
  | string -> let typ = name_to_eff string |> List.hd |> String.lowercase_ascii in 
              if typ = String.lowercase_ascii str 
              then make_type string else loop_lines str file_channel
  
(*[get_type str] is the Ptype.t representation of [str]. If str is not a valid
type, then MissingVal exception is raised*)
let get_type str = 
    Pervasives.open_in (dir^types)|> loop_lines str

  






let break_string_type str =
  str |> String.split_on_char 

(* [break_string str] breaks up the line [str] by commas, and is the list of strings
between each of the commas.*)
let break_string_poke str = 
  str |> String.split_on_char ','

(*[filter_poke str_lst] is the pokemon.t representation of a pokemon from the string
list str_lst*)
let filter_poke str_lst: Pokemon.t = 
  failwith "unimplemented"

