(* Basic *)

open Core.Std
open Printf

(* val length_of_list : 'a list -> float = <fun> *)
let length_of_list lst = 
  let rec lol sum lst =
    match lst with
    | [] -> sum
    | e :: rest -> lol (sum +. 1.) rest
  in lol 0. lst

(* val list_to_int : int list -> int = <fun> *)
let list_to_int lst = 
  let rec convert lst sum offset =
    match lst with
    | [] -> sum
    | digit :: rest -> convert rest (sum + (digit * offset)) (offset / 10)
  in convert lst 0 (int_of_float (10. ** ((length_of_list lst) -. 1.)))

let lst = [1;2;3]

let () = printf "\n"
let () = printf "list:\t"
let () = List.iter ~f:(printf "%d, ") lst
let () = printf "\n"
let () = printf "int:\t%d\n" (list_to_int lst)

(* val avg_num : int list -> float = <fun> *)
let avg_num lst = 
  let sum = List.fold lst 
  ~init:0
  ~f:(fun acc x -> acc + x) in
  let length = length_of_list lst in
  (float_of_int sum) /. length

let () = printf "sum:\t%f\n" (avg_num [1;2;5;3;6;2;8;3;4;2;1])

(* val exists_in_list : 'a list -> bool *)
let rec exists_in_list e l = 
  match l with
  | [] -> false
  | hd :: tl -> if hd = e then true else (exists_in_list e tl)

let () = printf "eil:\t%B\n" (exists_in_list 3 [1;2;3;4;5])
let () = printf "eil:\t%B\n" (exists_in_list 6 [1;2;3;4;5])

let dedup lst =
  List.fold lst
  ~init:[]
  ~f:(fun acc x -> if (exists_in_list x acc) then acc else (acc @ [x]))

let () = printf "dedup:\t"
let () = List.iter ~f:(printf "%d, ") (dedup [1;2;3;4;5;5;6;1;2])
let () = printf "\n"

let () = printf "%s" (Plist.stringify_s ["test";"strings";"now"])
let () = printf "%s" (Plist.stringify_f [1.;2.;3.])
let () = printf "%s" (Plist.stringify_i [1;2;3;4;5;6;7])
























