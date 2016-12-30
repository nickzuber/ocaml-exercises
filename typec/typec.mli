
(* Different forms of an expression, this is basically our AST structure *)
type expr = 
  | Num of int                                  (* Num 2 *)
  | Id of string                                (* Id "x" *)
  | Bool of bool                                (* Bool true *)
  | Binop of (int -> int -> int) * expr * expr  (* Binop (+) (Num 1) (Num 2) *)
  | Bif of bool * expr * expr                   (* Bif true (Num 1) (Num 2) *)
  | With of string * expr * expr                (* With "x" (Binop (+) (Id "x") (Num 2)) (Num 1) *)
  | App of expr * expr                          (* App (Binop (...)) (Num 3) *)
  | Fun of string * expr                        (* Fun "x" (...) *)

(* Valid types *)
type t = 
  | T_num
  | T_bool
  | T_fun

(* Parses string expressions into an AST *)
val parse : string -> expr

(* Type checks a given AST and if no type errors occur, provides expression type *)
val typeOf : expr -> t

