
(* ==================================== *)
(* TYPES DEFINITION                     *)
(* ==================================== *)

(** Represent the two lists of location IDs of the puzzle locations *)
type locations = { mutable left: int list; mutable right: int list}

(* ==================================== *)
(* PUZZLE PARSING                       *)
(* ==================================== *)

(** Inserts recursively a integer into a list sorted by ascending order. *)
let rec insert elem l : int list = match l with
| []                  -> elem :: []
| t::_ when elem < t  -> elem :: l
| t::q                -> t :: insert elem q

(** Extracts from each ligne of the puzzle input file the locations IDs and format them into a locations object. *)
let read_file filename = 
  let rec read_lines file locations =
    try
      let line = input_line file in
      let split_line = String.split_on_char ' ' line in
      begin
        locations.left <- insert (int_of_string (List.nth split_line 0)) locations.left;
        locations.right <- insert (int_of_string (List.nth split_line (List.length split_line - 1))) locations.right;
        read_lines file locations;
      end
    with End_of_file -> ()

  and file = open_in filename
  and locations = { left = []; right = [] } 
  in 

  begin
    read_lines file locations;
    close_in file;
    locations;
  end

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

(** Computes the sum of distances for each pair of location IDs. *)
let compute_total_distance locations = 
  let rec compute_distance left right = match (left, right) with
    | [],[]                             -> 0
    | [],_                              -> print_endline "Left locations list empty"; -1
    | _,[]                              -> print_endline "Right locations list empty"; -1
    | t_left::q_left, t_right::q_right  -> (abs (t_left-t_right)) + (compute_distance q_left q_right)
  in compute_distance locations.left locations.right

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)

(** In the locations right list, counts recursively the occurences of each element. *)
let rec traversal_right l memo = match l with
  | [] -> memo
  | t::q when Hashtbl.mem memo t -> 
    begin
      Hashtbl.replace memo t (Hashtbl.find memo t + 1); 
      traversal_right q memo;
    end
  | t::q -> 
    begin
      Hashtbl.add memo t 1; 
      traversal_right q memo
    end

(** Computes the similarity score recursively on the locations left list, using the result of traversal_right function. *)
let rec traversal_left l memo = match l with
| []                              -> 0
| t :: q when Hashtbl.mem memo t  -> (t * Hashtbl.find memo t) + traversal_left q memo
| _ :: q                          -> traversal_left q memo

(** Computes the similarity score of location IDs.*)
let compute_similarity locations = 
  let memo = traversal_right locations.right (Hashtbl.create 0)
  in traversal_left locations.left memo

(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let locations = read_file "./day01/day01.input" in 
  begin
    print_int (compute_total_distance locations);
    print_newline ();
    print_int (compute_similarity locations);
    print_newline ();
  end


(* =================================== *)