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
;;

(* val list_to_int : int list -> int = <fun> *)
let list_to_int lst = 
  let rec convert lst sum offset =
    match lst with
    | [] -> sum
    | digit :: rest -> convert rest (sum + (digit * offset)) (offset / 10)
  in convert lst 0 (int_of_float (10. ** ((length_of_list lst) -. 1.)))
;;

let lst = [1;2;3]

let () = printf "\n"
let () = printf "list:\t"
let () = List.iter ~f:(printf "%d, ") lst
let () = printf "\n"
let () = printf "int:\t%d\n" (list_to_int lst)

(* *)
let avg_num lst = 
  let sum = List.fold lst 
  ~init:0
  ~f:(fun acc x -> acc + x) in
  let length = length_of_list lst in
  (float_of_int sum) /. length
;;

let () = printf "sum:\t%f\n" (avg_num [1;2;5;3;6;2;8;3;4;2;1])

