
(* Representing the binary operations on numbers *)
type ops =
  | Plus
  | Minus
  | Mult
  | Div

(* Different forms of an expression, this is basically our AST structure *)
type expr = 
  | Num of int
  | Id of string
  | Bool of bool
  | Binop of ops * expr * expr
  | Bif of bool * expr * expr
  | With of string * expr * expr
  | App of expr * expr
  | Fun of string * expr

(* Valid types *)
type t = 
  | T_num
  | T_bool
  | T_fun

let parse sexp = 
  Binop (Plus,
        (Binop (Mult, (Num 1), (Num 2))),
        (With 
          ("x",
          (Binop (Minus, (Num 1), (Id "x"))),
          (Num 2))))

let sampleAST = parse "(+ (* 1 2) (with x (- 1 x) 2))"

let typeOf expr =
  T_num

let printType = function
  | T_num -> "NumberLiteral"
  | T_bool -> "Boolean"
  | T_fun -> "Function"

let () = Printf.printf "\n => %s\n" (printType (typeOf sampleAST))


