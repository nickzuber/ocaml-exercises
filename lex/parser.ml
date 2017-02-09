
module Token = Lexer.Token
open Batteries

let filename = Sys.argv.(1)

let env = []

let _ =
  let input = open_in filename in
  Printf.printf "\n%s\n" (BatInnerIO.read_all input)

let _ =
  let input = open_in filename in
  let filebuf = Lexing.from_input input in
  let result = Lexer.token env filebuf in
  let (complete_env, final_token) = result in
  let complete_env = List.rev complete_env in
  List.iter 
    (fun tok -> Printf.printf "%s " (Token.token_to_string tok))
    complete_env;
  Printf.printf "\n"

