
(* Functions, with, and arithmetic expressions *)
type sexp = 
  | Num of int
  | Binop
  | With
  | Id of string
  | Fun of string list * sexp
  | App of sexp * sexp list

(*  *)
type closure = {
  params: string list; (* the list of identifiers *)
  body: sexp;          (* body expression *)
  env: env;            (* function's scope *)
}

(* Interpreted value for a sexp *)
type sexp_value =
  | NumV of int
  | ClosureV of closure

(* Binding from symbol to its value *)
type binding = {
  name: string;
  value: sexp;
}

(* Empty environment *)
type mt_env = Empty

(* Environment scoping *)
type an_env = {
  name: string;
  value: sexp_value;
  next: env;
}

type env = 
  | MtEnv of mt_env
  | AnEnv of an_env
