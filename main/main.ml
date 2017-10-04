
module rec TestMethod : sig
  type t = A of int
  [@@deriving show]
end = TestMethod

let test = TestMethod.A 123

let _ = Format.printf "%a@." TestMethod.pp test
