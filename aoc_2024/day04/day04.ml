(* ==================================== *)
(* TYPES DEFINITION                     *)
(* ==================================== *)

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

(** Reverses a string.
  *
  * Params:
  * - str : String to reverse
  *)
let reverse str =
  let char_list = List.of_seq (String.to_seq str)
  in let reversed_char_list = List.rev char_list
  in String.of_seq (List.to_seq reversed_char_list)

(** Searches the number of occurence of a pattern and its corresponding reverse pattern into a string.
  *
  * Params:
  * - str     : String to process
  * - pattern : Regex to match
  *)
let search str pattern = 
  let aux str pattern =
    let regex = Re.Perl.compile_pat pattern
    in List.length (Re.matches regex str) 
  in aux str pattern + aux str (reverse pattern)

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

(** Counts all "XMAS" and "SAMX" in every line and every downward diagonal of the given matrix of char.
  * It analyzes the matrix according to each column j such that j = 4*k-1., with k integer such that 0 < k < number_of_columns/4.
  * For a given column, take an extract of each line i and an extract of each downward diagonal.
  * and searches the number of occurences of the patterns. 
  * NB: Each extract (line or diagonal) has a length of 7 and is centered to the point (i,j).
  *
  * Params:
  * - matrix : The matrix to process
  *)
let count_in_lines_and_downward_diags matrix = 

  let extract_from_downward_diag i j = 
    let rec aux i j before_stop = match i,j with
      | _,_ when before_stop = 0              -> []
      | i,_ when i < 0                        -> aux (i+1) (j+1) (before_stop-1)
      | i,_ when i >= Array.length matrix     -> []
      | i,j when j >= Array.length matrix.(i) -> aux (i+1) (j+1) (before_stop-1)
      | i,j                                   -> matrix.(i).(j) :: aux (i+1) (j+1) (before_stop-1)
    in String.of_seq (List.to_seq (aux (i-3) (j-3) 7))

  in let extract_from_line i j = 
    let rec aux i j before_stop = match j with
      | _ when before_stop = 0              -> []
      | j when j >= Array.length matrix.(i) -> []
      | j                                   -> matrix.(i).(j) :: aux i (j+1) (before_stop-1)
    in String.of_seq (List.to_seq (aux i (j-3) 7))

  in let evaluate_lines_and_downward_diags j =
    let rec aux i j = match i with
      | i when i = Array.length matrix  -> 0
      | i                               ->
          search (extract_from_line i j) "XMAS"
          + search (extract_from_downward_diag i j) "XMAS"
          + aux (i+1) j
    in aux 0 j

  in let rec count_in_lines_and_downward_diags_rec j = match j with
    | j when j >= Array.length matrix.(0) -> 0
    | j                                   -> 
        evaluate_lines_and_downward_diags j 
        + count_in_lines_and_downward_diags_rec (j+4)

  in count_in_lines_and_downward_diags_rec 3

(** Counts all "XMAS" and "SAMX" in every column and every upward diagonal of the given matrix of char.
  * It analyzes the matrix according to each column i such that i = 4*k-1., with k integer such that 0 < k < number_of_lines/4.
  * For a given line, take an extract of each column j and an extract of each upward diagonal.
  * and searches the number of occurences of the patterns. 
  * NB: Each extract (line or diagonal) has a length of 7 and is centered to the point (i,j).
  *
  * Params:
  * - matrix : The matrix to process
  *)
let count_in_columns_and_upward_diag matrix = 

  let extract_from_upward_diag i j = 
    let rec aux i j before_stop = match i,j with
      | _,_ when before_stop = 0              -> []
      | i,_ when i >= Array.length matrix     -> aux (i-1) (j+1) (before_stop-1)
      | i,j when j >= Array.length matrix.(i) -> []
      | _,j when j < 0                        -> aux (i-1) (j+1) (before_stop-1)
      | i,j                                   -> matrix.(i).(j) :: aux (i-1) (j+1) (before_stop-1)
    in String.of_seq (List.to_seq (aux (i+3) (j-3) 7))

  in let extract_from_column i j = 
    let rec aux i j before_stop = match i with
      | _ when before_stop = 0          -> []
      | i when i >= Array.length matrix -> []
      | i                               -> matrix.(i).(j) :: aux (i+1) j (before_stop-1)
    in String.of_seq (List.to_seq (aux (i-3) j 7))

  in let evaluate_columns_and_upward_diags i =
    let rec aux i j = match j with
      | j when j = Array.length matrix.(i)  -> 0
      | j                                   -> 
          search (extract_from_column i j) "XMAS"
          + search (extract_from_upward_diag i j) "XMAS"
          + aux i (j+1)
    in aux i 0

  in let rec count_in_columns_and_upward_diag_rec i = match i with
    | i when i >= Array.length matrix -> 0
    | i                               -> 
        evaluate_columns_and_upward_diags i 
        + count_in_columns_and_upward_diag_rec (i+4)

  in count_in_columns_and_upward_diag_rec 3

(** Counts all "XMAS" and "SAMX" in the given matrix of char.
  *
  * Params:
  * - matrix : The matrix to process
  *)
let count_xmas matrix = 
  count_in_lines_and_downward_diags matrix
  + count_in_columns_and_upward_diag matrix

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)

(** Searches in each line and each column all occurences of two MAS in the shape of an X.
  * Does it by looking for each case of the matrix that contains a 'A' and, for each of them, analyses the corresponding X shape.
  *
  * Awaited shape (or equivalent):
  * M S       M M       S M       S S
  *  A    or   A    or   A    or   A
  * M S       S S       S M       M M
  *
  * Params:
  * - matrix : The matrix to process
  *)
let count_mas matrix = 

  let extract_from_upward_diag i j = 
    let rec aux i j before_stop = match i,j with
      | _,_ when before_stop = 0              -> []
      | i,_ when i < 0                        -> []
      | i,_ when i >= Array.length matrix     -> aux (i-1) (j+1) (before_stop-1)
      | _,j when j < 0                        -> aux (i-1) (j+1) (before_stop-1)
      | _,j when j >= Array.length matrix.(i) -> []
      | i,j                                   -> matrix.(i).(j) :: aux (i-1) (j+1) (before_stop-1)
    in String.of_seq (List.to_seq (aux (i+1) (j-1) 3))

  in let extract_from_downward_diag i j = 
    let rec aux i j before_stop = match i,j with
      | _,_ when before_stop = 0              -> []
      | i,_ when i < 0                        -> aux (i+1) (j+1) (before_stop-1)
      | i,_ when i >= Array.length matrix     -> []
      | _,j when j < 0                        -> aux (i+1) (j+1) (before_stop-1)
      | _,j when j >= Array.length matrix.(i) -> []
      | i,j                                   -> matrix.(i).(j) :: aux (i+1) (j+1) (before_stop-1)
    in String.of_seq (List.to_seq (aux (i-1) (j-1) 3))

  in let evaluate_diags i j = match i,j with
    | i,j when matrix.(i).(j) != 'A'  -> 0
    | i,j                             -> 
        if
          search (extract_from_upward_diag i j) "MAS"
          + search (extract_from_downward_diag i j) "MAS"
          = 2
        then 1
        else 0

  in let rec count_mas_rec i = 
    let rec aux i j = match j with 
      | j when j >= Array.length matrix.(i) -> 0
      | j                                   -> evaluate_diags i j + aux i (j+1)
    in match i with
      | i when i >= Array.length matrix -> 0
      | i                               -> aux i 0 + count_mas_rec (i+1)

  in count_mas_rec 0

(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let matrix = read_file "./day04/day04.input" in 
  begin
    print_result (count_xmas matrix);
    print_result (count_mas matrix);
  end

(* =================================== *)
