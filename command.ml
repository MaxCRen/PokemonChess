type command_phrase = string list

type command = 
  |Use of command_phrase
  |Info of command_phrase
  |Quit

exception Empty
exception Incorrect

let break_string str =
  str |> String.lowercase_ascii

let parse_phrase string = 
  failwith"unimplemented"
