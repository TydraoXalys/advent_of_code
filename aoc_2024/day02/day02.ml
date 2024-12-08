(* ==================================== *)
(* TYPES DEFINITION                     *)
(* ==================================== *)

(* ==================================== *)
(* PUZZLE PARSING                       *)
(* ==================================== *)

(** Extracts data from puzzle input *)
let read_file filename = 
  let rec read_lines file =
    try
      let line = input_line file 
      in let int_list = List.map int_of_string (String.split_on_char ' ' line) 
      in int_list :: read_lines file;
    with End_of_file -> []

  and file = open_in filename
  in read_lines file

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

let is_safe report =
  let rec is_safe_rec report comparator = match report with
    | []                                            -> failwith "Empty report"
    | _ :: []                                       -> true
    | t1 :: t2 :: _ when abs (t1-t2) > 3            -> false
    | t1 :: t2 :: _ when t1 = t2                    -> false
    | t1 :: t2 :: _ when t1 > t2 && comparator = 1  -> false
    | t1 :: t2 :: _ when t1 < t2 && comparator = -1 -> false
    | _ :: q                                        -> is_safe_rec q comparator
  
  in match report with
    | []                                  -> failwith "Empty report"
    | _ :: []                             -> true
    | t1 :: t2 :: _ when abs (t1-t2) > 3  -> false
    | t1 :: t2 :: _ when t1 = t2          -> false
    | t1 :: t2 :: q when t1 > t2          -> is_safe_rec (t2::q) (-1)
    | t1 :: t2 :: q when t1 = t2          -> is_safe_rec (t2::q) 0
    | _ :: q                              -> is_safe_rec q 1


let rec count_safe reports = match reports with
| [] -> 0
| t :: q when is_safe t -> 1 + count_safe q
| _ :: q -> count_safe q

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)



(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let reports = read_file "./day02/day02.input" in 
  begin
    print_int (count_safe reports);
    print_newline ();
  end


(* =================================== *)