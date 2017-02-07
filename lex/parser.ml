
module Token = Lexer.Token
open Batteries

(* GENERAL NOTES ABOUT STRUCTURE:
 * 
 * The Lexer itself defines an environment
 *  - Maybe here is where the lexer position and process is tracked and handled?
 *  - I've noticed this env is being passed around after each token iteration,
 *    this must be the source of truth for lexing?
 *)

let filename = Sys.argv.(1)

let _ =
  try
    let input = open_in filename in
    let filebuf = Lexing.from_input input in
    let result = Lexer.token filebuf in
    Printf.printf "%s\n" (Token.token_to_string result)
  with Lexer.Eof ->
    exit 0

