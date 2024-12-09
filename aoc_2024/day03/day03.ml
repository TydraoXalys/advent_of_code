(* ==================================== *)
(* TYPES DEFINITION                     *)
(* ==================================== *)

(* ==================================== *)
(* PRINT FUNCTIONS                      *)
(* ==================================== *)

(** Improves print_int function by adding a print_newline *)
let print_result x =
  begin
    print_int x;
    print_newline ();
  end

(* ==================================== *)
(* COMMON FUNCTIONS                     *)
(* ==================================== *)

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
      in line :: read_lines file;
    with End_of_file -> []

  and file = open_in filename
  in read_lines file

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

(** Extracts the multiplication from the corrupted memory.
  *
  * Params:
  * - memory : The extract of the corrupted memory to analyse.
  *)
let find_multiplications memory = 
  let regex = Re.Perl.compile_pat "mul\\(\\d+,\\d+\\)" 
  in Re.matches regex memory

(** Finds the list of factors in a multiplication.
  *
  * Params:
  * - multiplication : The multiplication to analyse.
  *)
let find_factors multiplication =
  let regex = Re.Perl.compile_pat "\\d+" 
  in let matches = Re.matches regex multiplication
  in List.map int_of_string matches

(** Compute a single multiplication.
  *
  * Params:
  * - multiplication : The multiplication to compute.
  *)
let compute multiplication = 
  let rec compute_rec factors acc = match factors with
    | [] -> acc
    | t :: q -> compute_rec q (acc*t)

  in let factors = find_factors multiplication
  in compute_rec factors 1

(** Compute all multiplication in an extract of the corrupted memory.
  *
  * Params:
  * - memory : The extract of the corrupted memory to analyse.
  *)
let compute_multiplications memory =
  let rec compute_multiplications_rec multiplications acc = match multiplications with
    | [] -> acc
    | mult :: q -> compute_multiplications_rec q (acc + compute mult)

  in let multiplications = find_multiplications memory
  in compute_multiplications_rec multiplications 0

(** Analyses the memory extracts and compute the multiplications
  *
  * Params:
  * - memory : The full corrupted memory to analyse.
  *)
let rec process memory = match memory with
  | [] -> 0
  | t :: q -> compute_multiplications t + process q

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)

(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let memory = read_file "./day03/day03.input" in 
  begin
    print_result (process memory);
  end

(* =================================== *)
