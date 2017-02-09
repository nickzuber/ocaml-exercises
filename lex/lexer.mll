(* File lexer.mll *)
{
exception Eof
module Token = struct
  type t =
    | INT of int
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | LPAREN
    | RPAREN
    | EOL
    | EOF

  let token_to_string tok = 
    match tok with
    | INT n -> "INT(" ^ (string_of_int n) ^ ")"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | TIMES -> "TIMES"
    | DIV -> "DIV"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | EOL -> "EOL"
    | EOF -> "EOF"

  let add_to_ast tok ast =
    tok :: ast

end
open Token
}

rule token env = parse
  | [' ' '\t']        { token env lexbuf }     (* skip blanks *)
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
  | '-'               { env, MINUS }
  | '*'               { env, TIMES }
  | '/'               { env, DIV }
  | '('               { env, LPAREN }
  | ')'               { env, RPAREN }
  | eof               { env, EOF }

