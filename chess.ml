open Pokemon
open Battle
type color = Black | White
type holder_pokemon = Pokemon.t
type piece = 
  | Pawn of holder_pokemon
  | Rook of holder_pokemon
  | Knight of holder_pokemon
  | Bishop of holder_pokemon
  | Queen of holder_pokemon
  | King of holder_pokemon
type square = string * int
type game_piece = piece * color * square * bool 
type column = (int * (color * game_piece option)) list
type board = (string * column) list

exception InvalidSquare of square
exception InvalidMove


(** [colors_match piece1 piece2] is whether two chess pieces with colors [c1]
    and [c2], respectively, share the same color. *)
let colors_match (_,c1,_,_) (_,c2,_,_) = c1 = c2

(** [int_of_letter letter] is the nth place of the upper-case ASCII
    interpretation of letter after 65 *)
let int_of_letter ltr = 
  (String.get ltr 0 |> Char.uppercase_ascii |> Char.code) - 65

(** [letter_of_int col] returns the reverse operation of [int_of_letter] *)
let letter_of_int col = 
  65 |> (+) col |>  Char.chr |> Char.escaped

(** [is_valid_square square] returns true if [square] is valid, false 
    otherwise. *)
let is_valid_square (c, r) = 
  (r >= 1 && r <= 8) && ((int_of_letter c) >= 0 && (int_of_letter c) <= 7)

(** [get_piece board square] returns the piece at [square] on [board] if there
    is any. None, otherwise. Raises [InvalidSquare] if no such square *)
let get_piece board square = 
  if is_valid_square square then
    snd (snd((List.assoc ((fst square) |> String.uppercase_ascii) board 
              |> List.nth) ((snd square) - 1 )))
  else raise (InvalidSquare square)

let change_board_at_piece_square (p,c,(cs,rs),b) piece_opt board = 
  List.map (fun (ltr, column) -> 
      if ltr = cs then 
        (
          ltr, 
          List.map (fun (row, (color, pieceopt)) -> 
              if(row = rs) then (row, (color, piece_opt))
              else (row, (color, pieceopt))
            ) column
        )
      else (ltr, column)) board


let add_piece piece board = 
  change_board_at_piece_square piece (Some piece) board

let remove_piece piece board = change_board_at_piece_square piece None board

(** [update_piece piece square b] returns an updated game piece 
    based on piece *)
let update_piece (p,c,s,b) square bo = (p,c,square,bo)

let is_free board (c, r)  = 
  if r >= 1 && r <= 8 then 
    begin
      match List.assoc_opt c board with
      | None -> raise (InvalidSquare (c,r))
      | Some column -> (snd (snd(List.nth column (r - 1)))) = None
    end 
  else raise (InvalidSquare (c,r))


let can_capture (piece1, color1, _, _) (piece2, color2, _, _) =
  match piece2 with
  | King _ -> false
  | _ -> color1 <> color2

(** [is_king piece] returns true if [piece] is a king *)
let is_king = function King _  -> true | _ -> false 

(** [get_next_square slope acc square board] gets the open alleyway in [board]
    with respect to [square] based on [slope]. [acc] stores the resulting
    alleyway. [color] is used to determine on what kind of piece to stop *)
let rec get_next_square color (c,r) acc square board = 
  let next_square = ((square |> fst |> int_of_letter |> (+) c |> letter_of_int),
                     r + (snd square)) in 
  if not (is_valid_square next_square) then acc
  else match get_piece board next_square with
    | None -> get_next_square color (c,r) (next_square :: acc) next_square
                board (* if there are no pieces in the way, we add
                         the square onto our list and forge onward *)
    | Some (p,col,_,_) -> 
      if (color = col) then acc (* we shouldn't like to add
                                                 a square as viable, which
                                                 holds a king or a piece
                                                 with the same color *)
      else next_square :: acc

(** [get_open_diagonals board square] returns the diagonals centered at 
    [square] on [board], up to and including any pieces in the path *)
let get_open_diagonals board square color = 
  List.flatten 
    [
      (get_next_square color (1,1) [] square board);
      (get_next_square color (-1,-1) [] square board);
      (get_next_square color (-1, 1) [] square board);
      (get_next_square color (1, -1) [] square board)
    ] 

(** [get_open_horizontals_and_verticals board square color] returns the 
    horizontals and verticals  centered at [square] on [board], up to and
    including any pieces in the path *) 
let get_open_horizontals_and_verticals board square color = 
  List.flatten 
    [
      (get_next_square color (1,0) [] square board);
      (get_next_square color (-1, 0) [] square board);
      (get_next_square color (0, 1) [] square board);
      (get_next_square color (0, -1) [] square board)
    ] 

(** [x_away square1 square2 distance_pair] returns true if [square1] 
    differs from [square2] by <= [disatnce_pair.fst] in its column and
    <= [distance_pair.snd] in its row *) 
let x_away (cs1, rs1) (c, r) (cs2, rs2) = 
  let cs1i = int_of_letter cs1 in 
  let cs2i = int_of_letter cs2 in 
  (abs (cs1i - cs2i)) <= c && (abs (rs1 - rs2)) <= r 

(** [piece_move piece board dist_pair] returns either an empty list 
    or a singleton list of squares, which are the squares resulting
    from adding the [dist_pair] coordinates to [piece] coordinates. *)
let piece_move (piece, color, (cl, r), moved) board (cx, ry) strict = 
  let c = int_of_letter cl in 
  try 
    match get_piece board ((letter_of_int (c + cx)), r + ry) with
    | None ->
      if strict then []
      else [(letter_of_int (c + cx)), r + ry] 
    | Some (p, color2, _, _) -> 
      if color = color2 then []
      else [(letter_of_int (c + cx)), r + ry]
  with InvalidSquare sq -> []

let get_moves ((piece, color, (cl,r), moved) as gamepiece)  board = 
  match piece with 
  | Pawn _ -> begin
      let multiplier = if color = White then 1 else -1 in
      let corners = 
        piece_move gamepiece board (1,(multiplier * 1)) true
        @
        piece_move gamepiece board (-1,(multiplier * 1)) true in 
      if moved then 
        piece_move gamepiece board (0,(multiplier * 1)) false @ corners
      else  
        piece_move gamepiece board (0,(multiplier * 1)) false
        @
        piece_move gamepiece board (0,(multiplier * 2)) false
        @
        corners
    end 
  | Rook _ -> get_open_horizontals_and_verticals board (cl,r) color
  | Knight _ -> 
    piece_move gamepiece board (1,2) false @
    piece_move gamepiece board (1,-2) false @
    piece_move gamepiece board (-1,2) false @
    piece_move gamepiece board (-1,-2) false @
    piece_move gamepiece board (2,1) false @
    piece_move gamepiece board (2,-1) false @
    piece_move gamepiece board (-2,1) false @
    piece_move gamepiece board (-2,-1) false
  | Bishop _ -> get_open_diagonals board (cl, r) color
  | Queen _ -> 
    get_open_diagonals board (cl, r) color @
    get_open_horizontals_and_verticals board (cl, r) color
  | King _ -> 
    let too_many = 
      get_open_diagonals board (cl, r) color @
      get_open_horizontals_and_verticals board (cl, r) color in 
    List.filter (x_away (cl, r) (1,1)) too_many 

let can_move piece board square = 
  List.mem square (get_moves piece board)

let move ((p,c,s,b) as piece) board square = 
  board |> remove_piece piece |> add_piece (p,c,square,b)

let pokemon_from_piece = function
  | None -> failwith "no pokemon"
  | Some 
      (Pawn p | Rook p | Knight p | Bishop p | Queen p | King p) -> p

module type Game = sig 
  type t 
  val new_game : t
  val move : square -> square -> t -> piece option * piece option * t option * t
  val as_list : t -> (square * piece option) list list
end 

module ChessGame : Game = struct

  type t = 
    {
      white : square;
      black : square;
      board : board;
      current_player : color
    }

  let rec make_pawns color acc num = 
    let row = (if color = Black then 7 else 2) in 
    if num >= 0 then 
      make_pawns color ((Pawn (Pokemon.get_pawn ()), color, 
                         ((letter_of_int num), row), false)::acc)
        (num - 1)
    else acc

  let white_pieces = 
    (make_pawns White [] 7 )
    @
    [(Rook (Pokemon.get_rook ()),  White, ("A",1), false); 
     (Knight (Pokemon.get_knight ()), White, ("B", 1), false); 
     (Bishop (Pokemon.get_bishop ()), White, ("C",1), false); 
     (Queen (Pokemon.get_queen()), White, ("D", 1), false);
     (King (Pokemon.get_king()), White, ("E",1), false); 
     (Bishop (Pokemon.get_bishop()), White, ("F",1), false); 
     (Knight (Pokemon.get_knight()), White, ("G",1), false); 
     (Rook (Pokemon.get_rook()), White, ("H",1), false)]

  let black_pieces = 
    (make_pawns Black [] 7)
    @
    [(Rook (Pokemon.get_rook()),  Black, ("A",8), false); 
     (Knight (Pokemon.get_knight()), Black, ("B", 8), false); 
     (Bishop (Pokemon.get_bishop()), Black, ("C",8), false); 
     (Queen (Pokemon.get_queen()), Black, ("D", 8), false);
     (King (Pokemon.get_king()), Black, ("E",8), false); 
     (Bishop (Pokemon.get_bishop()), Black, ("F",8), false); 
     (Knight (Pokemon.get_knight()), Black, ("G",8), false); 
     (Rook (Pokemon.get_rook()), Black, ("H",8), false)]

  let rec make_board board = function
    | [] -> board
    | h :: t -> make_board (add_piece h board) t

  let column_type_one : column =     [(1,(Black, None)); (2,(White, None)); 
                                      (3,(Black, None)); (4,(White, None));
                                      (5,(Black, None)); (6,(White, None));
                                      (7,(Black, None)); (8,(White, None))]

  let column_type_two : column = [(1,(White, None)); (2,(Black, None));
                                  (3,(White, None)); (4,(Black, None)); 
                                  (5,(White, None)); (6,(Black, None)); 
                                  (7,(White, None)); (8,(Black, None))]
  let empty : board = 
    [("A",column_type_one); ("B",column_type_two);
     ("C", column_type_one); ("D",column_type_two); 
     ("E",column_type_one); ("F", column_type_two); 
     ("G", column_type_one); ("H", column_type_two)]

  (** [new_game] returns a new instance of a chess game *)
  let new_game = 
    {
      white = ("E",1);
      black = ("E",8);
      board = make_board empty (white_pieces @ black_pieces);
      current_player = White;
    }

  let get_current_board chess = chess.board

(*
  let get_contestants piece1 piece2 =
    (get_poke piece1, get_poke piece2)*)

  (** [do_battle piece1 piece2] is called when [piece1] is attempting to 
      capture [piece2]. Successful if [piece1] defeats [piece2] in battle *)
  let do_battle piece1 piece2 = true

  let move square1 square2 ({white;black;board;current_player} as current) = 
    let next_col = (if current_player = White then Black else White) in
    match get_piece board square1 with
    | None -> raise InvalidMove
    | Some ((p1,c,(c1,r1),moved) as piece1) ->
      if current_player = c then
        let possible_moves = get_moves piece1 board in 
        if List.mem square2 possible_moves then 
          let change_moved = if not moved then not moved else moved in 
          let new_board = 
            (board |> remove_piece piece1 
             |> add_piece (p1,c,square2,change_moved))
          in
          let new_game = { 
            white = 
              if current_player = White then
                (if not (is_king p1) then white else square2)
              else white; 
            black = 
              if current_player = Black then
                (if not (is_king p1) then black else square2)
              else black;
            board = new_board;
            current_player = next_col
          } in 
          match get_piece board square2 with
          | None -> (Some p1, None, None, new_game)
          | Some (p2,_,_,_) ->
            (Some p1, Some p2, Some new_game, current)
        else raise InvalidMove
      else raise InvalidMove

  let as_list ({white;black;board;current_player} : t) = 
    let rec helper builder = function 
      | [] -> builder
      | (s,c) :: t -> 
        helper ((
            List.map (
              fun (num, (color, piece)) ->
                match piece with
                | None -> ((s,num),None)
                | Some (p,c,sq,b) -> (sq, Some p)
            ) c 
          )
            :: builder) t in 
    (helper [] board) |> List.rev
end 
