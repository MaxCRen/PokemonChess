open Pokemon
(** the OCaml representation of a chess game *)


(** [color] is the color of a piece or square, either black or white *)
type color = Black | White

type holder_pokemon = Pokemon.t

(** representation of a chess piece *)
type piece = Pawn of holder_pokemon
           | Rook of holder_pokemon
           | Knight of holder_pokemon
           | Bishop of holder_pokemon
           | Queen of holder_pokemon
           | King of holder_pokemon

(** [square] represents the square of a chess [board] indexed at the
    square's string and int. Ex: A8, H3, B7 *)
type square = string * int

(** [game_piece] represents a piece in a game of chess, having the attributes
    of a certain type of piece, a color, a square to indicate where it is, 
    and a bool to determine if the piece has moved yet *)
type game_piece = piece * color * square * bool

(** [column] is a list of alternating colors with the possibility of having a
    [game_piece] on it *)
type column = (int *(color * game_piece option)) list

(** [board] is the representation type of a chess board, i.e. a list of 
    [column] with [string] labels "A-H" *)
type board = (string * column) list 

(** [exception] raised on a [square] whose [fst] is <1 or >8 and whose 
    [snd] is < "A" or > "H" *)
exception InvalidSquare of square

(** [exception] raised if an invalid move is attempted to be made. *)
exception InvalidMove

(** [colors_match piece1 piece2] returns true if the color of piece1
    is the same as the color of piece2. False otherwise *)
val colors_match : game_piece -> game_piece -> bool

(** [is_free board square] returns true if there is no [game_piece] on 
    [square] in [board]
    Raises [InvalidSquare square] if [square] is invalid *)
val is_free : board -> square -> bool

(** [get_piece board square] returns the [game_piece] at [square] on [board] *)
val get_piece : board -> square -> game_piece option

(** [add_piece board piece] returns an updated board based on [board] with 
    [piece] added *)
val add_piece : game_piece -> board -> board 

(** [can_capture piece1 piece2] returns true if piece1 can capture piece2 *)
val can_capture : game_piece -> game_piece -> bool

(*
val get_contestants : piece -> piece -> (holder_pokemon * holder_pokemon)
*)

(** [get_moves piece board] returns a list of possible locations on [board]
    to which [piece] can move *)
val get_moves : game_piece -> board -> square list

(** [can_move piece board square] returns true if [piece] can move to 
    [square] on the given [board] *)
val can_move : game_piece -> board -> square ->  bool 

(** [move piece board square] returns an updated board with [piece] moved
    to [square] on [board] *) 
val move : game_piece -> board -> square -> board

(** [pokemon_from_piece piece] returns the [Pokemon.t] inside of [piece] *)
val pokemon_from_piece : piece option -> holder_pokemon

(** [get_sq_pair str] is the [square] represented by [str]. 
        Requires: [str] must represent a valid chess board coordinate (ex: ["A2"],
        ["C8"], etc.) *)
val get_sq_pair : string -> square

(** [Game] represents an active chess_game *)
module type Game = sig 
  type t 

  (* 
(** [check game] returns true if a king is under check *)
 val check :  t -> bool

(** [mate game] returns true if a king has been mated *) 
 val mate : t -> bool *)

  (**[new_game] returns a new game *)
  val new_game : t

  (** [move square1 square2 game] returns the game resulting from moving piece
      at [square1] to [square2] *)
  val move : square -> square -> t -> piece option * piece option * t option * t

  (** [get_moves game square] returns a list of possible moves for the piece
      at [square], if there is any, in [game]. If there is no piece there,
      the empty list is returned. *)
  val get_moves : t -> square -> square list

  (** [is_player_square t sq] returns whether the piece on [sq] is one of the
      pieces belonging to the current player of [t]. If there is no piece on 
      [sq], [false] is returned. *)
  val is_player_square : t -> square -> bool

  (** [as_list game] returns a list representation of [game] *)
  val as_list : t -> (square * piece option * color option * color) list list

end 

module ChessGame : Game
