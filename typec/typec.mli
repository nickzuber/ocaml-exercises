
(* Representing the binary operations on numbers *)
type ops =
  | Plus
  | Minus
  | Mult
  | Div

(* Different forms of an expression, this is basically our AST structure *)
type expr = 
  | Num of int                                  (* Num 2 *)
  | Id of string                                (* Id "x" *)
  | Bool of bool                                (* Bool true *)
  | Binop of ops * expr * expr                  (* Binop (Plus, (Num 1), (Num 2)) *)
  | Bif of expr * expr * expr                   (* Bif (Bool true, (Num 1), (Num 2)) *)
  | With of string * expr * expr                (* With ("x", (Num 1), (Binop (Plus, (Id "x"), (Num 2))) *)
  | App of expr * expr                          (* App ((Binop (...)), (Num 3)) *)
  | Fun of string * expr                        (* Fun ("x", (...)) *)

(* Valid types *)
type t = 
  | T_num
  | T_bool
  | T_fun of t * t   (* Represents the type of the single paramter and the return type*)
  | T_arb of string  (* Represents some arbitrary type that can be anything *)
  | T_error of string

(* Binding from an identifier to the type of its value *)
type binding = {
  name: string;
  mutable value: t;
} 

(* An environment or closure consisting of bindings *)
type env =
  | Mt
  | Env of binding * env

(* Parses string expressions into an AST *)
val parse : string -> expr

(* Type checks a given AST and if no type errors occur, provides expression type *)
val typeOf : expr -> t

