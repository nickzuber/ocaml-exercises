
module Token = Lexer.Token
include Token
open Batteries

let __FILE__ = true

let filename = Sys.argv.(1)

let env = {
  Token.
  state = [REGULAR];
  exprs = [[]];
  ast = []
}

(* Print file *)
let _ =
  if __FILE__ then
    let input = open_in filename in
    Printf.printf "\n%s" (BatInnerIO.read_all input)


(* AST *)
let print_ast env = 
  Printf.printf "[ ";
  List.iter 
    (fun tok -> Printf.printf "%s; " (Token.token_to_string tok))
    (List.rev env.ast);
  Printf.printf "] \n\n"

(* Parse file *)
let parse_from_file () =
  let input = open_in filename in
  let filebuf = Lexing.from_input input in
  let result = Lexer.token env filebuf in
  let (env, final_token) = result in
  print_ast env
let parse_from_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  let result = Lexer.token env lexbuf in
  let (env, final_token) = result in
  print_ast env

(*
let _ = 
  while true do
    let i = read_int () in
    print_endline (string_of_int i)
  done
*)

let _ = 
  if __FILE__ = true then
    parse_from_file ()
  else 
    parse_from_stdin ()

