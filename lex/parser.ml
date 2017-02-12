
module Token = Lexer.Token
include Token
open Batteries

let filename = Sys.argv.(1)

let env = {
  Token.
  state = [REGULAR];
  exprs = [[]];
  ast = []
}

(* Print file *)
let _ =
  let input = open_in filename in
  Printf.printf "\n%s" (BatInnerIO.read_all input)

(* Parse file*)
let _ =
  let input = open_in filename in
  let filebuf = Lexing.from_input input in
  let result = Lexer.token env filebuf in
  let (env, final_token) = result in
  (* AST *)
  Printf.printf "[ ";
  List.iter 
    (fun tok -> Printf.printf "%s; " (Token.token_to_string tok))
    (List.rev env.ast);
  Printf.printf "] \n";

