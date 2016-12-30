
(* Functions, with, and arithmetic expressions *)
type cfwae = 
  | Num of int
  | Binop
  | With
  | Id of string
  | Fun
  | App

(* Interpreted value for a cfwae *)
type cfwae_value =
  | NumV of int
  | ClosureV

(* Binding from symbol to its value *)
type binding = {
  name: string;
  value: cfwae;
}

(* Empty environment *)
type mt

(* Environment scoping *)
type env = {
  name: string;
  value: cfwae_value;
  next: env;
}
