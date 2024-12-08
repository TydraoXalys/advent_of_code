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

(** Checks if the first rule is broken.
  * NB: The first rule say that the levels are either all increasing or all decreasing.
  * 
  * Params:
  * - lvl1, lvl2 : Levels to check.
  * - comparator : Indicates if levels must be all increasing (=1) or decreasing (=-1).
  *)
let is_fst_rule_broken lvl1 lvl2 comparator =
  (lvl1 > lvl2 && comparator <= -1) || (lvl1 < lvl2 && comparator >= 1) 

(** Checks if the second rule is broken.
  * NB: The second rule say that the difference between two adjacent levels must be between 1 and 3.
  * 
  * Params:
  * - lvl1, lvl2 : Levels to check.
  *)
let is_snd_rule_broken lvl1 lvl2 =
  (abs (lvl1-lvl2) > 3) || (lvl1 = lvl2) 

(** Checks if one of the two rules is broken.
  * 
  * Params:
  * - lvl1, lvl2 : Levels to check.
  * - comparator : Indicates if levels must be all increasing or decreasing.
  *)
let are_rules_broken lvl1 lvl2 comparator =
  (is_fst_rule_broken lvl1 lvl2 comparator) || (is_snd_rule_broken lvl1 lvl2)

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
      in let int_list = List.map int_of_string (String.split_on_char ' ' line) 
      in int_list :: read_lines file;
    with End_of_file -> []

  and file = open_in filename
  in read_lines file

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

(** Checks if a report is completely safe (no bad level). 
  *
  * Params:
  * - report : List of integers that represent a report.
  *)
let is_safe report = 

  (*  Execute the next iteration of the report analysis in Part One.
    *
    * Params:
    * - report      : List of integers that represent the remaining part of the report to analyse.
    * - comparator  : Indicates if levels must be all increasing or decreasing.
    *)
  let rec next_iteration report comparator = match report with
    | []                                                            -> failwith "Empty report"
    | _ :: []                                                       -> true
    | lvl1 :: lvl2 :: _ when are_rules_broken lvl1 lvl2 comparator  -> false
    | _ :: q                                                        -> next_iteration q comparator
  
  in match report with
    | []                                                  -> failwith "Empty report"
    | _ :: []                                             -> true
    | lvl1 :: lvl2 :: _ when is_snd_rule_broken lvl1 lvl2 -> false
    | lvl1 :: lvl2 :: q                                   -> next_iteration (lvl2::q) (compare lvl1 lvl2)

(** Counts the number of safe reports.
  *
  * Params:
  * - reports : List that contains all reports.
  *)
let rec count_safe reports = match reports with
  | []                    -> 0
  | t :: q when is_safe t -> 1 + count_safe q
  | _ :: q                -> count_safe q

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)

(** Gets the comparator that tells if levels must be all increasing or decreasing.
  *
  * Params:
  * - report : List of integers that represent the report to analyse.
  *)
let get_comparator report =
  let rec get_comparator_rec report acc = match report with
    | [] -> 0
    | _ :: [] -> 0
    | _ when acc = 4 -> 0
    | lvl1 :: lvl2 :: q -> (get_comparator_rec (lvl2::q) (acc+1)) + (compare lvl1 lvl2)
  in get_comparator_rec report 0

(** Execute the next iteration of the report analysis in Part Two.
  *
  * Params:
  * - report        : List of integers that represent the remaining part of the report to analyse.
  * - previous_lvl  : Level that is before lvl1, the head of the list 'report'. This level and lvl1 satisfy the two rules by definition.
  * - comparator    : Indicates if levels must be all increasing or decreasing.
  * - bad_lvl_found : Boolean that indicates if a bad level has been found during previous iterations.
  *)
let rec next_iteration report previous_lvl comparator bad_lvl_found = 

  (*  Checks if the report remains safe in this iteration if we remove one level.
    *
    * Params:
    * - previous_lvl  : Level that is before lvl1 in the entire report. This level and lvl1 satisfy the two rules by definition.
    * - lvl1, lvl2    : Levels that are analysed in this iteration.
    * - next_lvl      : Level that follows lvl2 in the entire report.
    * - comparator    : Indicates if levels must be all increasing or decreasing.
    * - bad_lvl_found : Boolean that indicates if a bad level has been found during previous iterations.
    * - report        : List of integers that represent the remaining part of the report to analyse.
    *)
  let check_iteration previous_lvl lvl1 lvl2 next_lvl comparator bad_lvl_found report =

    (*  Gets the value of previous_lvl for the next iteration.
      *
      * Params:
      * - lvl1, lvl2    : Levels that are analysed in this iteration.
      * - next_lvl      : Level that follows lvl2 in the entire report.
      * - comparator    : Indicates if levels must be all increasing or decreasing.
      *)
    let get_previous_level lvl1 lvl2 next_lvl comparator =
      if 
        are_rules_broken lvl1 next_lvl comparator
      then
        lvl2
      else
        lvl1 
    
    (* Code of check_iteration() *)
    in if (
      bad_lvl_found || (
        (
          are_rules_broken previous_lvl lvl2 comparator ||
          are_rules_broken lvl2 next_lvl comparator
        ) && 
        are_rules_broken lvl1 next_lvl comparator
      )
    )
    then
      false
    else
      let previous = get_previous_level lvl1 lvl2 next_lvl comparator
      in next_iteration (next_lvl::report) previous comparator true
  
  (* Code of next_iteration() *)
  in match report with
    | []                                                                        -> failwith "Empty report"
    | _ :: []                                                                   -> true
    | lvl1 :: lvl2 :: []                                                        -> not (bad_lvl_found && (are_rules_broken lvl1 lvl2 comparator))
    | lvl1 :: lvl2 :: next_lvl :: q when are_rules_broken lvl1 lvl2 comparator  -> check_iteration previous_lvl lvl1 lvl2 next_lvl comparator bad_lvl_found q
    | lvl1 :: q                                                                 -> next_iteration q lvl1 comparator bad_lvl_found

(** Checks if a report is safe, taking into account The Problem Dampener (0 or 1 bad level). 
  *
  * Params:
  * - report : List of integers that represent a report.
  *)
let is_safe_with_dampener report =

  (*  Checks if the report is safe at the first iteration if we remove the first or the second level.
    *
    * Params:
    * - lvl1, lvl2    : Levels that are analysed in this iteration.
    * - next_lvl      : Level that follows lvl2 in the entire report.
    * - comparator    : Indicates if levels must be all increasing or decreasing.
    * - report        : List of integers that represent the remaining part of the report to analyse.
    *)
  let check_first_iteration lvl1 lvl2 next_lvl comparator report =

    (*  Gets the value of previous_lvl for the second iteration.
      *
      * Params:
      * - lvl1, lvl2    : Levels that are analysed in this iteration.
      * - next_lvl      : Level that follows lvl2 in the entire report.
      * - comparator    : Indicates if levels must be all increasing or decreasing.
      *)
    let get_previous_level lvl1 lvl2 next_lvl comparator =
      if 
        are_rules_broken lvl1 next_lvl comparator
      then 
        lvl2
      else 
        lvl1
    
    (* Code of check_first_iteration() *)
    in if 
      are_rules_broken lvl1 next_lvl comparator && 
      are_rules_broken lvl2 next_lvl comparator
    then 
      false
    else
      let previous = get_previous_level lvl1 lvl2 next_lvl comparator
      in next_iteration (next_lvl::report) previous comparator true
  
  (* Code of is_safe_with_dampener() *)
  in let comparator = get_comparator report
  in if comparator = 0 then false else match report with
    | []                                                                        -> failwith "Empty report"
    | _ :: []                                                                   -> true
    | _ :: _ :: []                                                              -> true
    | lvl1 :: lvl2 :: next_lvl :: q when are_rules_broken lvl1 lvl2 comparator  -> check_first_iteration lvl1 lvl2 next_lvl comparator q
    | lvl1 :: lvl2 :: q                                                         -> next_iteration (lvl2::q) lvl1 comparator false


(** Counts the number of safe reports, taking into account The Problem Dampener (0 or 1 bad level). 
  *
  * Params:
  * - reports : List that contains all reports.
  *)
let rec count_safe_with_dampener reports = match reports with
  | []                                            -> 0
  | report :: q when is_safe_with_dampener report -> 1 + count_safe_with_dampener q
  | _ :: q                                        -> count_safe_with_dampener q

(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let reports = read_file "./day02/day02.input" in 
  begin
    print_result (count_safe reports);
    print_result (count_safe_with_dampener reports);
  end

(* =================================== *)
