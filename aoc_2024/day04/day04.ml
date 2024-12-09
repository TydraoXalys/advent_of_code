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



(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)



(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let data = read_file "./day04/day04.input" in 
  begin
    print_result 0;
    print_string data
  end

(* =================================== *)
