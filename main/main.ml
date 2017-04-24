
(* Doing examples and excersises for the sake of learning OCaml *)
(*
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
*)


module rec BinaryExpression : sig
  type t = {
    left: Expression.t;
    right: Expression.t;
  }
end = BinaryExpression

and NumberExpression : sig
  type t = int
end = NumberExpression

and Expression : sig
  type t = 
    | BinaryExpression of BinaryExpression.t
    | NumberExpression of NumberExpression.t
end = Expression

open BinaryExpression

let ast = Expression.BinaryExpression {
  left = Expression.NumberExpression 1;
  right = Expression.BinaryExpression {
    left = Expression.NumberExpression 1;
    right = Expression.NumberExpression 2;
  }
}


