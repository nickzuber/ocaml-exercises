(* File lexer.mll *)
{
exception Eof
module Token = struct
  type t =
    | INT of int
    | PLUS
    | MINUS
    | MULT
    | DIV
    | EXPR of t list
    | EOL
    | EOF

  type state_t = 
    | REGULAR
    | STARTING_EXPR
    | INSIDE_EXPR
    | ENDING_EXPR

  type env_t = {
    mutable state: state_t list;
    mutable exprs: t list list;
    mutable ast: t list;
  }

  let rec token_to_string tok = 
    match tok with
    | INT n -> "INT(" ^ (string_of_int n) ^ ")"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | MULT -> "MULT"
    | DIV -> "DIV"
    | EXPR el -> "EXPR(" ^ 
      (List.fold_right (fun e acc -> acc ^ (token_to_string e) ^ " ") el " ") 
      ^ ")"
    | EOL -> "EOL"
    | EOF -> "EOF"

  let state_to_string = function
    | REGULAR -> "REGULAR"
    | STARTING_EXPR -> "STARTING_EXPR"
    | INSIDE_EXPR -> "INSIDE_EXPR"
    | ENDING_EXPR -> "ENDING_EXPR"

  (* Update the lexer state *)
  let update_state state env =
    env.state <- state :: env.state; env

  (* Add a token to the current expression being build *)
  let add_to_cur_expr tok env =  
    let new_exprs = 
      match env.exprs with
      | [] -> [[tok]]
      | expr :: rest -> (tok :: expr) :: rest
    in env.exprs <- new_exprs; env
 

  (* Add a token to the ast *)
  let rec add_to_ast tok env =
    match List.hd env.state with
    | STARTING_EXPR -> update_state INSIDE_EXPR env; add_to_ast tok env
    | ENDING_EXPR -> update_state REGULAR env; add_to_ast tok env
    | INSIDE_EXPR -> add_to_cur_expr tok env
    | _ -> env.ast <- (tok :: env.ast); env

  (* Add a new empty expression to the current expression front *)
  let add_new_expr env =
    env.exprs <- [] :: env.exprs; env

  (* Remove the front expression and add it to the ast *)
  let consume_expr env =
    let new_env =
      match env.exprs with
      | [] -> { 
        state = env.state; 
        exprs = [[]]; 
        ast = env.ast; }
      | expr :: rest -> { 
        state = env.state;
        exprs = rest;
        ast = ((EXPR(expr)) :: env.ast) }
    in env.exprs <- new_env.exprs;
    env.ast <- new_env.ast; env

end
open Token
}

rule token env = parse
  | [' ' '\t']        { token env lexbuf }
  | ['\n' ]           { 
                        let env = add_to_ast EOL env in
                        token env lexbuf
                      }
  | ['0'-'9']+ as lxm { 
                        let env = add_to_ast (INT(int_of_string lxm)) env in
                        token env lexbuf
                      }
  | '+'               {
                        let env = add_to_ast PLUS env in
                        token env lexbuf
                      }
  | '-'               {
                        let env = add_to_ast MINUS env in
                        token env lexbuf
                      }
  | '*'               {
                        let env = add_to_ast MULT env in
                        token env lexbuf
                      }
  | '/'               {
                       let env = add_to_ast DIV env in
                        token env lexbuf
                      }
  | '('               { 
                        let env = add_new_expr env in
                        let env = update_state STARTING_EXPR env in
                        token env lexbuf
                      }
  | ')'               { 
                        let env = consume_expr env in
                        let env = update_state ENDING_EXPR env in
                        token env lexbuf
                      }
  | eof               { env, EOF }

