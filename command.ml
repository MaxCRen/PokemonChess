

type command_phrase = string

type command = 
  | Use of command_phrase
  | Move of command_phrase * command_phrase
  | Help
  | Info of command_phrase
  | Quit
  | Incorrect


exception Empty

(** [break_string str] breaks string [str] into its space-delimited 
    component parts *)
let break_string str =
  str |> String.split_on_char ' '


let parse_phrase str = 
  match break_string str with 
  | [] -> raise Empty
  | "use"::t when List.length t >= 1 ->
    let init_str = List.fold_left (fun acc rt -> acc ^ " " ^ rt) "" t in
    Use(String.sub init_str 1 ((String.length init_str) - 1))
  | h::t when h = "use" -> Use("")
  | h::t when h = "move" ->
    let init_str = List.fold_left (fun acc rt -> acc ^ " " ^ rt) "" t in
    let cmd = break_string init_str in
    (match cmd with
     | h :: x1 :: x2 :: [] -> Move (x1,x2)
     | _ -> Incorrect)
  | h::t when h = "info" -> 
    Info(List.fold_left (fun acc rt -> acc ^ "" ^ rt) "" t)
  | h::t when h = "help" -> Help
  | h::t when h = "quit" -> Quit
  | _::_ -> Incorrect

(** [check_coordinate coord] is whether ot not [coord] is a valid coordinate
    corresponding to a square on a chess board, in which columns are labeled
    A..H and rows are labeled 1..8. *)
let check_coordinate coord =
  if String.length coord <> 2 then false
  else let row_num = Pervasives.int_of_char coord.[1] - 48 in
    List.mem (Char.lowercase_ascii coord.[0]) ['a';'b';'c';'d';'e';'f';'g';'h'] 
    && row_num >= 1 && row_num <= 8