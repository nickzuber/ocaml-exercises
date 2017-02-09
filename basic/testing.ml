
let lst = [[7; 5]; [7; 3; 2]]

let join_plus lst =    
  let rec inner_join lst acc =
    match lst with   
    | [] -> acc
    | n :: [] -> acc ^ (string_of_int n)
    | n :: rest -> inner_join rest (acc ^ (string_of_int n) ^ " + ")
  in inner_join lst ""

let stringify_all_lists lst =
  List.fold_right (fun lst acc -> (join_plus lst) :: acc) lst []

let () = List.iter 
  (fun l -> Printf.printf "%s\n" l)
  (stringify_all_lists lst)
