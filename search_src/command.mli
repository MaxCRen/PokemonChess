type command_phrase = string

(** [command] is a variant containing the various types of commands 
    used in a Pokemon battle *)
type battle_command = 
  | Use of command_phrase
  | Help
  | Info of command_phrase
  | Quit
  | Incorrect
  (** [exception] if a command is empty *)
type chess_command = 
  | Move of command_phrase * command_phrase
  | Incorrect
  | Quit
exception Empty

(** [parse_phrase str] parses the string [str] into the commands it may
    or may not contain *)
val parse_phrase_battle : string -> battle_command

val parse_phrase_chess : string -> chess_command

val check_coordinate : string -> bool