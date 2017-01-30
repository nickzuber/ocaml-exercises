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
end
open Token
}
rule token = parse
  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }

