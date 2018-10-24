type command_phrase = string

type command = 
  |Use of command_phrase
  |Help
  |Info of command_phrase
  |Quit
  |Incorrect

exception Empty


val parse_phrase: string -> command