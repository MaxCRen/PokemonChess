

type command_phrase = string

type command = 
  |Use of command_phrase
  |Help
  |Info of command_phrase
  |Quit
  |Incorrect


exception Empty

let break_string str =
  str |> String.lowercase_ascii |> String.split_on_char ' '

let parse_phrase str = 
  match break_string str with 
  |[] -> raise Empty
  |h::t when h = "use" && List.length t > 1->
                         Use(List.fold_left (fun acc rt -> acc^" "^rt) "" t)
  |h::t when h = "use" -> Use(List.hd t)
  |h::t when h = "info" -> Info(List.fold_left (fun acc rt -> acc^""^rt) "" t)
  |h::t when h = "help" -> Help
  |h::t when h = "quit" -> Quit
  |_::_ -> Incorrect

