
{
exception SyntaxError of string

module Token = struct
  type t =
    | NUMBER of int
    | TRUE
    | FALSE
    | LEFT_BRACE
    | RIGHT_BRACE
    | LEFT_BRACK
    | RIGHT_BRACK
    | COLON
    | COMMA
    | SEMI
    | NULL
    | EOF
end
open Token
}

let integer = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let floating = digit* frac? exp?

let number = integer | floating

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = 
  parse
  | white   { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lextbuf }
  | number  { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }


