(**Our representation of a command is a string *)
type command_phrase = string

(** [battle_command] is a variant containing the various types of commands 
    used in a Pokemon battle *)
type battle_command = 
  | Use of command_phrase
  | Help
  | Quit
  | Incorrect

(**[chess_command] is a variant containing the various types of commands
    used for a chess game *)
type chess_command = 
  | Square of command_phrase
  | Cancel
  | Incorrect
  | Quit

(** [exception] if a command is empty *)
exception Empty

(** [parse_phrase str] parses the string [str] into the commands it may
    or may not contain *)
val parse_phrase_battle : string -> battle_command

val parse_phrase_chess : string -> chess_command

val check_coordinate : string -> bool