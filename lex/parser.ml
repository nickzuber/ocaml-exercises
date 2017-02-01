
module Token = Lexer.Token

(* GENERAL NOTES ABOUT STRUCTURE:
 * 
 * The Lexer itself defines an environment
 *  - Maybe here is where the lexer position and process is tracked and handled?
 *  - I've noticed this env is being passed around after each token iteration,
 *    this must be the source of truth for lexing?
 *
 *)

let _ =
  try
    let open Printf in
    let lexbuf = Lexing.from_channel stdin in
    let result = Lexer.token lexbuf in
    printf "%s\n" (Token.token_to_string result);
    printf "lexbuf: %d\n" (Lexing.lexeme_start lexbuf)
  with Lexer.Eof ->
    exit 0

