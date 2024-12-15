(* ==================================== *)
(* TYPES DEFINITION                     *)
(* ==================================== *)

type direction = NORTH | SOUTH | EAST | WEST

(* ==================================== *)
(* PRINT FUNCTIONS                      *)
(* ==================================== *)

(** Improves print_int function by adding a print_newline *)
let print_result x = 
  print_int x; 
  print_newline ()

(* ==================================== *)
(* COMMON FUNCTIONS                     *)
(* ==================================== *)

(** Gets the coordinates of the start point.
  * 
  * Params:
  * - matrix : The matrix of the puzzle.
  *)
let find_start matrix =
  let rec find_in_elements i j = match j with
    | j when j >= Array.length matrix.(i) -> -1
    | j when matrix.(i).(j) = '^'         -> j
    | j                                   -> find_in_elements i (j+1)
  in let rec find_in_lines i = match i with
    | i when i >= Array.length matrix -> failwith "No start found."
    | i                               -> let j = find_in_elements i 0 in if j != -1 then (i,j) else find_in_lines (i+1)
  in find_in_lines 0

(* ==================================== *)
(* PUZZLE PARSING                       *)
(* ==================================== *)

(** Extracts data from puzzle input 
  *
  * Params:
  * - filename : Path to the file.
  *)
let read_file filename = 
  let rec read_lines file =
    try
      let line = input_line file 
      in (Array.of_seq (String.to_seq line)) :: read_lines file;
    with End_of_file -> []

  and file = open_in filename
  in Array.of_list (read_lines file)

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

(** Checks the point (i,j) in the grid and increment the accumulator (if this point is visited for the first time).
  * Returns the updated accumulator.
  *
  * Params:
  * - matrix  : The matrix of the puzzle.
  * - i,j     : Coordinates of the point to check.
  * - acc     : Accumulator to increment if the condition is verified.
  *)
let check_position_and_update_acc matrix i j acc = match matrix.(i).(j) with
| 'X' -> acc
| '^' -> acc
| _   -> matrix.(i).(j) <- 'X'; acc+1

(** Compute the moves of the guard and returns the number of unique positions that are visited.
  *
  * Params:
  * - matrix  : The matrix of the puzzle.
  * - i,j     : Coordinates of the current position.
  * - i,j     : Direction the guard is currently looking to.
  * - acc     : Accumulator that store the number of unique positions that are visited.
  *)
let rec move_no_custom_obstruction matrix i j direction acc = 

  let keep_direction acc = match direction with
    | NORTH -> move_no_custom_obstruction matrix (i-1) j direction acc
    | SOUTH -> move_no_custom_obstruction matrix (i+1) j direction acc
    | EAST  -> move_no_custom_obstruction matrix i (j+1) direction acc
    | WEST  -> move_no_custom_obstruction matrix i (j-1) direction acc
  in

  let change_direction acc = match direction with
    | NORTH -> move_no_custom_obstruction matrix i (j+1) EAST acc
    | SOUTH -> move_no_custom_obstruction matrix i (j-1) WEST acc
    | EAST  -> move_no_custom_obstruction matrix (i+1) j SOUTH acc
    | WEST  -> move_no_custom_obstruction matrix (i-1) j NORTH acc
  in

  let finish acc = acc
  in

  let handle_next_move () = 
    
    let has_reach_border i j direction =
      (direction = NORTH && i = 0) 
      || (direction = SOUTH && i = Array.length matrix-1)
      || (direction = EAST && j = Array.length matrix.(i)-1)
      || (direction = WEST && j = 0)
    in 
    
    let has_reach_exising_obstruction i j direction =
      (direction = NORTH && matrix.(i-1).(j) = '#') 
      || (direction = SOUTH && matrix.(i+1).(j) = '#')
      || (direction = EAST && matrix.(i).(j+1) = '#')
      || (direction = WEST && matrix.(i).(j-1) = '#')
    in 
    
    match i,j,direction with
      | i,j,direction when has_reach_border i j direction               -> finish (check_position_and_update_acc matrix i j acc)
      | i,j,direction when has_reach_exising_obstruction i j direction  -> change_direction (check_position_and_update_acc matrix i j acc)
      | _,_,_                                                           -> keep_direction (check_position_and_update_acc matrix i j acc)
  
  in handle_next_move ()

(** Starts the computation of the number of unique positions visited by the guard.
  *
  * Params:
  * - matrix  : The matrix of the puzzle.
  *)
let count_positions matrix =
  let (i,j) = find_start matrix in move_no_custom_obstruction matrix i j NORTH 1

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)



(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let matrix = read_file "./day06/day06.input" in 
  begin
    print_result (count_positions matrix);
  end

(* =================================== *)
