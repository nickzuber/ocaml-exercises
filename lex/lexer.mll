(* File lexer.mll *)
{
let __DEBUG__ = true
let __FILE__ = true
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
    | EXPR el -> "EXPR("^ 
      (List.fold_left (fun acc e -> acc ^ (token_to_string e) ^ "; ") " " el) 
      ^ ")"
    | EOL -> "EOL"
    | EOF -> "EOF"

  let state_to_string = function
    | REGULAR -> "REGULAR"
    | STARTING_EXPR -> "STARTING_EXPR"
    | INSIDE_EXPR -> "INSIDE_EXPR"
    | ENDING_EXPR -> "ENDING_EXPR"

  (* Just prints the `exprs` from an environment *)
  let debug_env env = 
    Printf.printf "env = {\n";
    (* state *)
    let state_string = (List.fold_left 
      (fun acc s -> acc ^ (state_to_string s) ^ "; ") "" env.state) in
    Printf.printf "  state = %s\n" state_string;
    (* exprs *)
    Printf.printf "  exprs = [ ";
    List.iter
      (fun expr ->
        Printf.printf "[ ";
        List.iter 
          (fun tok -> Printf.printf "%s; " (token_to_string tok))
          expr;
        Printf.printf "]") 
      env.exprs;
    (* ast *)
    Printf.printf " ]\n  ast = [ ";
    List.iter
      (fun tok -> Printf.printf "%s; " (token_to_string tok))
      (List.rev env.ast);
      Printf.printf "]\n}\n\n"

  (* Update the lexer state *)
  let update_state state env =
    env.state <- state :: env.state; env

  (* Add a token to the current expression being built *)
  let add_to_cur_expr tok env =  
    let new_exprs = 
      match env.exprs with
      | [] -> [[tok]]
      | expr :: [] -> [(expr @ [tok])]
      | expr :: rest -> [(expr @ [tok])] @ rest
    in env.exprs <- new_exprs; env
 

  (* Add a token to the ast *)
  let rec add_to_ast tok env =
    match List.hd env.state with
    | STARTING_EXPR -> 
        let env = update_state INSIDE_EXPR env in
        add_to_ast tok env
    | INSIDE_EXPR -> add_to_cur_expr tok env
    | _ -> env.ast <- (tok :: env.ast); env

  (* Add a new empty expression to the current expression front *)
  let add_new_expr env =
    match env.exprs with
    | [] -> env.exprs <- [] :: env.exprs; env
    | [] :: [] -> env.exprs <- env.exprs; env
    | _ -> env.exprs <- [] :: env.exprs; env

  (* Close current working expression and append it to the first of the 
   * rest expressions list *)
  let update_exprs expr rest =
    let head = (List.hd rest) @ [(EXPR(expr))] in
    head :: (List.tl rest)

  (* Remove the front expression and either add it to the ast or 
   * close current working expression and append it to the next in line *)
  let consume_expr env =
    (* NOTE: we only ever call this function when are ending an expression,
     * so we can handle state changing out of expression context here. *)
    let new_env =
      match env.exprs with
      (* Consuming an empty expression, almost definitely means we're closing
       * the final expression after just closing the second to last one in the
       * context of some nested expressions.
       * Ex.
       *   (1 + (2 + 3)) + 4
       *               ^
       *)
      | [] -> { 
          state = REGULAR :: env.state; 
          exprs = [[]]; 
          ast = env.ast;
        }
      (* Consuming the final expression in a sequence of expressions. This is
       * when the previous token was a non-expression terminating character.
       * Ex.
       *   (1 + (2 + 3) + 4) + 5 
       *                   ^
       *)
      | expr :: [] -> { 
          state = REGULAR :: env.state;
          exprs = [[]];
          ast = (EXPR(expr)) :: env.ast
        }
      | expr :: rest -> {
          state = INSIDE_EXPR :: env.state;
          exprs = update_exprs expr rest;
          ast = env.ast;
        }
    in env.exprs <- new_env.exprs;
    env.ast <- new_env.ast;
    env.state <- new_env.state; env

end
open Token
}

rule token env = parse
  | [' ' '\t']        { 
                        if __DEBUG__ = true then ( debug_env env );
                        token env lexbuf
                      }
  | ['\n' ]           { 
                        if __FILE__ then
                          let env = add_to_ast EOL env in
                          token env lexbuf
                        else
                          env, EOL
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
                        let env = update_state STARTING_EXPR env in
                        let env = add_new_expr env in
                        token env lexbuf
                      }
  | ')'               {  
                        let env = update_state ENDING_EXPR env in
                        let env = consume_expr env in
                        token env lexbuf
                      }
  | eof               { env, EOF }

