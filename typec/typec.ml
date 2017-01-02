
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
  | Null

(* Valid types *)
type t = 
  | T_num
  | T_bool
  | T_fun

type binding = {
  name: string;
  value: expr;
}

let parse sexp = 
  Id "Not implemented"

let sampleAST = 
  Binop (Plus,
        (Binop (Mult, (Num 1), (Num 2))),
        (With 
          ("x",
          (Binop (Minus, (Num 1), (Id "x"))),
          (Num 2))))

let simpleAST = 
  Bool true

let rec resolve id env = 
  match env with
  | [] -> Null
  | bind :: rest when bind.name = id -> bind.value
  | bind :: rest -> resolve id rest

let typeOf expr = 
  let rec getType e env = 
    match e with
    | Num n -> T_num
    | Bool b -> T_bool
    | Id id -> getType (resolve id env) env
  in getType expr []

let printType = function
  | T_num -> "NumberLiteral"
  | T_bool -> "Boolean"
  | T_fun -> "Function"

let () = Printf.printf "\n => %s\n" (printType (typeOf simpleAST))


