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

let () = printf "LST: "
let () = List.iter ~f:(printf "%d, ") lst
let () = printf "\n"
let () = printf "INT: %d\n" (list_to_int lst)

