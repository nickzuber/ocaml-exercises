
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
  | T_error of string

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
  Binop (Plus, (Num 1), (Bool true))

let rec resolve id env = 
  match env with
  | [] -> Null
  | bind :: rest when bind.name = id -> bind.value
  | bind :: rest -> resolve id rest

(* Checks if a given type is a T_num *)
let isNum t = 
  match t with
  | T_num -> true
  | _ -> false

let typeOf expr = 
  let rec getType e env = 
   match e with
    | Null -> T_error "Bad expression."
    | Num n -> T_num
    | Bool b -> T_bool
    | Id id -> getType (resolve id env) env
    | Binop (ops, lhs, rhs) -> 
        let lhs' = getType lhs env in
        let rhs' = getType rhs env in
        if isNum lhs' then
          if isNum rhs' then
            T_num
          else T_error "Binop right hand side was not a number."
        else T_error "Binop left hand side was not a number."
    | Bif (cond, th, el) -> T_error "Not implemented"
    | With (id, value, body) -> T_error "Not implemented"
    | App (body, args) -> T_error "Not implemented"
    | Fun (param, body) -> T_error "Not implemented"
  in getType expr []

let printType t =
  match t with
  | T_num -> "NumberLiteral"
  | T_bool -> "Boolean"
  | T_fun -> "Function"
  | T_error t -> "TypeError: " ^ t

let () = Printf.printf "\n => %s\n" (printType (typeOf simpleAST))


