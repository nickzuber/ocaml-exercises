
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
  | Bif of expr * expr * expr
  | With of string * expr * expr
  | App of expr * expr
  | Fun of string * expr

(* Valid types *)
type t = 
  | T_num
  | T_bool
  | T_fun
  | T_error of string

type binding = {
  name: string;
  value: t;
}

type env = 
  | Mt
  | Env of binding * env

let extend_env env b = 
  match env with
  | Mt -> Env (b, Mt)
  | Env (fb, re) as e -> Env (b, e)

let parse sexp = 
  Id "Not implemented"

let sampleAST = 
  Binop (Plus,
        (Binop (Mult, (Num 1), (Num 2))),
        (With 
          ("x",
          (Num 2),
          (Binop (Minus, (Num 1), (Id "x"))))))

let simpleAST = 
  With ("x",
       (Num 2),
       (Binop (Minus, (Num 1), (Id "y"))))

let rec resolve id env = 
  match env with
  (* I don't think anything is done with this error at the moment. It's just ignored *)
  | Mt -> T_error "Bad expression. Was probably unable to resolve an identifier."
  | Env (b, e) when b.name = id -> b.value
  | Env (b, e) -> resolve id e

let caughtError t = 
  match t with 
  | T_error msg -> true
  | _ -> false

let typeOf expr = 
  let rec getType e env = 
   match e with
    | Num n -> T_num
    | Bool b -> T_bool
    | Id id -> resolve id env
    | Binop (ops, lhs, rhs) -> 
        let lhs' = getType lhs env in
        let rhs' = getType rhs env in
        if lhs' = T_num then
          if rhs' = T_num then
            T_num
          else if caughtError rhs' then rhs'
          else T_error "Binop right hand side was not a number."
        else if caughtError lhs' then lhs'
        else T_error "Binop left hand side was not a number."
    | Bif (cond, th, el) -> 
        let cond' = getType cond env in
        let th' = getType th env in
        let el' = getType el env in
        if cond' = T_bool then
          if th' = el' then
            th' (* Since both th' and el' are equal, it doesn't matter which one we return here *)
          else if caughtError th' then th'
          else if caughtError el' then el'
          else T_error "Bif branches were of different types."
        else T_error "Bif condition was not a boolean."
    | With (id, value, body) -> 
        let value' = getType value env in
        let newBinding = { name = id; value = value' } in
        let env' = extend_env env newBinding in
        getType body env'
    | App (body, args) -> T_error "Not implemented"
    | Fun (param, body) -> T_error "Not implemented"
  in getType expr Mt

let printType t =
  match t with
  | T_num -> "NumberLiteral"
  | T_bool -> "Boolean"
  | T_fun -> "Function"
  | T_error t -> "TypeError: " ^ t

let () = Printf.printf "\n => %s\n" (printType (typeOf sampleAST))


