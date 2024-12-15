(* ==================================== *)
(* TYPES DEFINITION                     *)
(* ==================================== *)

(** Represents all rules such that "page" is before each element of "pages_after" *)
type rule = { page: string; mutable pages_after: string list}

(** Represents the formatted input *)
type input = { mutable rules : rule list; mutable updates : string list }

(** Check if there is a rule instance corresponding to the given page.
  *
  * Params:
  * - page  : The page to check.
  * - rules : The list of existing rule instances.
  *)
let rec rule_mem page rules = match rules with
  | []                        -> false
  | t :: _ when t.page = page -> true
  | _ :: q                    -> rule_mem page q

(** Adds a new element in the "pages_after" list of the corresponding rule instance.
  *
  * Params:
  * - page_after  : The page to add.
  * - rules       : The list of existing rule instances.
  * - page        : The page associated to the rule instance.
  *)
let rec rule_add page_after rules page = match rules with
  | []                        -> failwith "Rule not found"
  | t :: _ when t.page = page -> t.pages_after <- t.pages_after @ [page_after]
  | _ :: q                    -> rule_add page_after q page

(** Returns the "page_after" list of the corresponding rule instance.
  *
  * Params:
  * - rules : The list of existing rule instances.
  * - page  : The page associated to the rule instance.
  *)
let rec get_pages_after rules page = match rules with
| []                        -> []
| t :: _ when t.page = page -> t.pages_after
| _ :: q                    -> get_pages_after q page

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

(** Return the middle page number of an update.
  *
  * Params:
  * - update : The update to process.
  *)
  let get_middle_page update = int_of_string (List.nth update (List.length update / 2))

(** Checks if the reversed update extract does not contains the element of the pages_after list given.
  *
  * Params:
  * - rule_pages_after  : List of pages number that must not be in the rev_update list.
  * - rev_update        : The reversed list of page numbers associated to an extract of an update.
  *)
let rec check rule_pages_after rev_update = match rule_pages_after with
  | []    -> true
  | t::q  -> (not (List.mem t rev_update)) && check q rev_update
  
(** Checks if an update complies with the rules.
  *
  * Params:
  * - update  : The update to process.
  * - rules   : The list of rule instances to comply with.
  *)
let is_correct update rules =
  let rec aux rev_update = match rev_update with
    | [] -> true
    | t :: q -> (check (get_pages_after rules t) q) && (aux q)
  in aux (List.rev (String.split_on_char ',' update)) 

(* ==================================== *)
(* PUZZLE PARSING                       *)
(* ==================================== *)

(** Handles a rule represented by the given file line by adding the right member
  * in the "page_after" list of the rule instance corresponding to the left member.
  * Creates the corresponding instance of rule if it does not exist yet.
  * 
  * Params:
  * - line : The file line that represents the update to add.
  * - data : Input instance.
  *)
let handle_rule line data =
  let line_as_list = String.split_on_char '|' line in 

  let page = List.hd line_as_list 
  and page_after = List.nth line_as_list (List.length line_as_list - 1)
  in 
  
  (if not (rule_mem page data.rules) then 
    let new_rule = { page = page ; pages_after = []} 
    in data.rules <- List.append data.rules [new_rule]);
  rule_add page_after data.rules page

(** Adds an update represented by the given file line into the corresponding list of input instance.
  * 
  * Params:
  * - line : The file line that represents the update to add.
  * - data : Input instance.
  *)
let handle_update line data = 
  data.updates <- List.append data.updates [line]

(** Extracts data from puzzle input 
  *
  * Params:
  * - filename : Path to the file.
  *)
let read_file filename = 

  let data = { rules = [] ; updates = [] }

  in let rec read_lines file section =
    try
      let line = input_line file 
      in match line with
        | ""                  -> read_lines file 2
        | _ when section = 1  -> handle_rule line data; read_lines file section
        | _                   -> handle_update line data; read_lines file section
    with End_of_file -> data

  and file = open_in filename
  in read_lines file 1

(* ==================================== *)
(* PART ONE                             *)
(* ==================================== *)

(** Compute the sum of middle page numbers associated to the corrects updates.
  *
  * Params:
  * - update  : The update to process.
  * - rules   : The list of rule instances to comply with.
  *)
let compute_correct updates rules =
  let rec aux updates rules acc = match updates with
    | []                            -> acc
    | t::q when is_correct t rules  -> aux q rules (acc + get_middle_page (String.split_on_char ',' t))
    | _::q                          -> aux q rules acc
  in aux updates rules 0

(* ==================================== *)
(* PART TWO                             *)
(* ==================================== *)

(** Checks if the "page_after" page must be after "page" according to the rules.
  *
  * Params:
  * - page_after  : The page to process.
  * - page        : The page of the corresponding rule instance to check.
  * - rules       : The list of rule instances.
  *)
let rec is_page_after page_after page rules = match rules with
  | []                        -> false
  | t :: _ when t.page = page -> List.mem page_after t.pages_after
  | _ :: q                    -> is_page_after page_after page q

(** Reorders an incorrect update according to the rules.
  * Based on the insertion sort.
  *
  * Params:
  * - update  : The update to process.
  * - rules   : The list of rule instances to comply with.
  *)
let rec order update rules =
  
  let rec insert page update = match update with
    | []                                            -> [page]
    | t :: q when not (is_page_after page t rules)  -> page :: t :: q
    | t :: q                                        -> t :: insert page q

  in match update with
    | []    -> []
    | t::q  -> insert t (order q rules)

(** Compute the sum of middle page numbers associated to the corrects updates.
  *
  * Params:
  * - update  : The update to process.
  * - rules   : The list of rule instances to comply with.
  *)
  let compute_incorrect updates rules =
    let rec aux updates rules acc = match updates with
      | []                            -> acc
      | t::q when is_correct t rules  -> aux q rules acc
      | t::q                          -> aux q rules (acc + get_middle_page (order (String.split_on_char ',' t) rules))
    in aux updates rules 0

(* ==================================== *)
(* MAIN                                 *)
(* ==================================== *)

let () =
  let data = read_file "./day05/day05.input" in 
  begin
    print_result (compute_correct data.updates data.rules);
    print_result (compute_incorrect data.updates data.rules);
  end

(* =================================== *)
