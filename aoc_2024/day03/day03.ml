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

(** Extracts from the corrupted memory all strings that match the given pattern.
  *
  * Params:
  * - memory  : The corrupted memory to process.
  * - regex   : The pattern to match.
  *)
let find memory pattern = 
  let regex = Re.Perl.compile_pat pattern
  in Re.matches regex memory

(** Finds the list of factors in a multiplication.
  *
  * Params:
  * - multiplication : The multiplication to process.
  *)
  let find_factors multiplication =
    let regex = Re.Perl.compile_pat "\\d+" 
    in let matches = Re.matches regex multiplication
    in List.map int_of_string matches
  
(** Compute a single multiplication.
  * Uses terminal recursion.
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
      in line ^ read_lines file;
    with End_of_file -> ""

  and file = open_in filename
  in read_lines file

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

(** Computes the given list of multiplications.
  *
  * Params:
  * - multiplications : The list of multiplications to compute.
  * - acc             : Accumulator that sum the multiplications results
  *)
let rec compute_multiplications multiplications acc = match multiplications with
  | [] -> acc
  | mult :: q -> compute_multiplications q (acc + compute mult)

(** Process the entire corrupted memory to compute multiplications.
  *
  * Params:
  * - memory : The corrupted memory to process.
  *)
let process memory = 
  let pattern = "mul\\(\\d+,\\d+\\)"  
  in compute_multiplications (find memory pattern) 0

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)

(** Computes the given list of multiplications.
  *
  * Params:
  * - multiplications : The list of multiplications to compute.
  * - dont            : Boolean that indicates if future mul instructions are disabled by a "dont()" or not.
  * - acc             : Accumulator that sum the multiplications results
  *)
let rec compute_multiplications multiplications dont acc = match multiplications with
  | [] -> acc
  | t :: q when dont -> 
      if t = "do()" then (compute_multiplications q false acc) 
      else (compute_multiplications q dont acc)
  | t :: q -> 
      if t = "don't()" then (compute_multiplications q true acc) 
      else if t = "do()" then (compute_multiplications q dont acc)
      else (compute_multiplications q dont (acc + compute t))

(** Process the entire corrupted memory to compute multiplications, taking do() and don't() into account.
  *
  * Params:
  * - memory : The corrupted memory to process.
  *)
let process_with_do_dont memory = 
  let pattern = "(do\\(\\))|(don't\\(\\))|(mul\\(\\d+,\\d+\\))"
  in compute_multiplications (find memory pattern) false 0

(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let memory = read_file "./day03/day03.input" in 
  begin
    print_result (process memory);
    print_result (process_with_do_dont memory);
  end

(* =================================== *)
