
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
  | T_fun of t * t   (* Represents the type of the single paramter and the return type*)
  | T_arb of string  (* Represents some arbitrary type that can be anything *)
  | T_error of string

let rec printType t =
  match t with
  | T_num -> "number"
  | T_bool -> "boolean"
  | T_fun (p, r) -> "function : " ^ (printType p) ^ " -> " ^ (printType r)
  | T_arb x -> "(\"" ^ x ^ "\" = " ^ "a')"
  | T_error t -> "TypeError: " ^ t

type binding = {
  name: string;
  mutable value: t;
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

let rec resolve id env = 
  match env with
  | Mt -> T_error "Bad expression. Was probably unable to resolve an identifier."
  | Env (b, _) when b.name = id -> b.value
  | Env (_, e) -> resolve id e

let caughtError t = 
  match t with 
  | T_error _ -> true
  | _ -> false

let caughtArb t =
  match t with
  | T_arb _ -> true
  | _ -> false

let getArbName t =
  match t with
  | T_arb p -> p
  | _ -> "Unknown identifier name"

let rec updateBinding v id env =
  match env with
  | Mt -> T_error "Couldn't find identifier when updating type. Should never get here."
  | Env (b, _) when b.name = id -> (b.value <- v); v
  | Env (_, e) -> updateBinding v id e

let typeOf expr = 
  let rec getType e env = 
   match e with
    | Num _ -> T_num
    | Bool _ -> T_bool
    | Id id -> resolve id env
    | Binop (ops, lhs, rhs) -> 
        let lhs' = getType lhs env in
        let rhs' = getType rhs env in (* I'm fixing the type too late. do BEFORE check *)
        if lhs' = T_num then
          if rhs' = T_num then
            T_num
          else if caughtError rhs' then rhs'
          else if caughtArb rhs' then updateBinding T_num (getArbName rhs') env
          else T_error ("Binop right hand side was not a number, instead we found: " ^ (printType rhs'))
        else if caughtError lhs' then lhs'
        else if caughtArb lhs' then updateBinding T_num (getArbName lhs') env
        else T_error ("Binop left hand side was not a number, instead we found: " ^ (printType lhs'))
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
    | Fun (param, body) ->  
        let initBinding = { name = param; value = T_arb param } in
        let env' = extend_env env initBinding in
        let body' = getType body env' in
        let param' = resolve param env' in
        T_fun (param', body')
  in getType expr Mt

let ast = 
  Fun ("x", (Binop (Plus, (Id "x"), 
                          (Bool true))))

let () = Printf.printf "\n => %s\n" (printType (typeOf ast))




