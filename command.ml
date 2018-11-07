

type command_phrase = string

type battle_command = 
  | Use of command_phrase
  | Help
  | Info of command_phrase
  | Quit
  | Incorrect

type chess_command = 
  | Square of command_phrase
  | Cancel
  | Incorrect
  | Quit

exception Empty

(** [break_string str] breaks string [str] into its space-delimited 
    component parts *)
let break_string str =
  str |> String.split_on_char ' '

let parse_phrase_battle str = 
  match break_string str with 
  | [] -> raise Empty
  | "use"::t when List.length t >= 1 ->
    let init_str = List.fold_left (fun acc rt -> acc ^ " " ^ rt) "" t in
    Use(String.sub init_str 1 ((String.length init_str) - 1))
  | "info"::t -> 
    Info(List.fold_left (fun acc rt -> acc ^ "" ^ rt) "" t)
  | "help"::t  -> Help
  | "quit"::t -> Quit
  | _::_ -> Incorrect

let parse_phrase_chess str = 
  match break_string str with
  | [] -> raise Empty
  | "cancel" :: t -> Cancel
  | "quit" :: t -> Quit
  | h :: [] -> if String.length h <> 2 then Incorrect else Square(h)
  | _::_ -> Incorrect

(** [in_range col h t] is whether or not the ASCII code of [col] falls 
    within the range [h]..[t]. *)
let in_range h t col =
  let ascii_code = Char.code col in
  ascii_code >= h && ascii_code <= t

(** [check_coordinate coord] is whether ot not [coord] is a valid coordinate
    corresponding to a square on a chess board, in which columns are labeled
    A..H and rows are labeled 1..8. *)
let check_coordinate coord =
  if String.length coord <> 2 then false
  else in_range 97 104 (Char.lowercase_ascii coord.[0]) 
       && in_range 49 56 coord.[1]