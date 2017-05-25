
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

(*
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
*)

module Stack : sig
  type 'a t = Node of 'a * 'a t | Nil
  val create : 'a -> 'a t
  val push : 'a t -> 'a -> 'a t
  val pop : 'a t -> 'a t
  val size : 'a t -> int
end = struct
  type 'a t = 
    | Node of 'a * 'a t
    | Nil
  let create n = Node (n, Nil)
  let push s n = Node (n, s)
  let pop s = 
    match s with
    | Node (n, s) -> s
    | Nil -> Nil
  let size s = 
    let rec _size s n =
      match s with 
      | Node (_, s) -> _size s (n + 1)
      | Nil -> n
    in _size s 0
end

let rec print_stack s =
  let open Stack in
  match s with
  | Node (v, n) -> 
    print_endline v;
    print_stack n
  | Nil -> ()

let _ = 
  print_endline "";
  let stack = Stack.create "first in" in
  let stack = Stack.push stack "first push" in
  let stack = Stack.push stack "second push" in
  let stack = Stack.pop stack in
  let size = Stack.size stack in
  print_stack stack;
  let open Core in
  print_endline (string_of_int size)





