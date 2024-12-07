type location_lists = { mutable left: int list; mutable right: int list}
let filename = "./day01/day01.example"

let rec insert elem l = match l with
| []                  -> elem :: []
| t::_ when elem < t  -> elem :: l
| t::q                -> t :: insert elem q

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

let compute_total_distance locations = 
  let rec compute_distance left right = match (left, right) with
    | [],[]                             -> 0
    | [],_                              -> print_endline "Left locations list empty"; -1
    | _,[]                              -> print_endline "Right locations list empty"; -1
    | t_left::q_left, t_right::q_right  -> (abs (t_left-t_right)) + (compute_distance q_left q_right)
  in compute_distance locations.left locations.right

let () =
  let input = read_file filename
  in print_int (compute_total_distance input)
