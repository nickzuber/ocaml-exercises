
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Lexer.token lexbuf in
    let open Printf in
    printf "%s\n" (Lexer.Token.token_to_string result)
(* 
    List.iter (fun tok -> printf "%s\n" (Lexer.Token.token_to_string tok)) result 
*)
  with Lexer.Eof ->
    exit 0

