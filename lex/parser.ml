let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Lexer.token lexbuf in
    let open Printf in
    printf "testing\n"
  with Lexer.Eof ->
    exit 0

