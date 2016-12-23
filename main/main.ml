
(* Doing examples and excersises for the sake of learning OCaml *)

open Printf

let rec fib n =
  if n < 2 then 1
  else fib (n - 1) + fib (n - 2)

let fib_list n =
  let rec create_list m = 
    if m = n then fib m :: []
    else fib m :: create_list (m + 1)
  in create_list 0

let () = List.iter (printf "  %d\n") (fib_list 12);;

printf "\n";;

