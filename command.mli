type command_phrase = string

(** [command] is a variant containing the various types of commands 
    used in a Pokemon battle *)
type command = 
  |Use of command_phrase
  |Help
  |Info of command_phrase
  |Quit
  |Incorrect
(** [exception] if a command is empty *)
exception Empty

(** [parse_phrase str] parses the string [str] into the commands it may
    or may not contain *)
val parse_phrase: string -> command
