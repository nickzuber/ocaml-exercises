
module B_DEFINITION = struct
  let bar x = x
    |> A.foo
    |> string_of_int
  let baz x = x + 10
end

module A_DEFINITION = struct
  let foo x = 
    let n = x + 1
    in B.baz n
end


(* --- *)

module rec A : sig
  val foo : int -> int
end = A_DEFINITION

and B : sig
  val bar : int -> string
  val baz : int -> int
end = B_DEFINITION

let test = B.bar 1

let _ = Printf.printf "result: %s\n" test 
