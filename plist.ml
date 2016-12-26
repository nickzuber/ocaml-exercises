
let rec stringify_s l = 
  let rec create_string l str = 
    match l with
    | [] -> str
    | [hd] -> str ^ hd ^ "\n"
    | hd :: tl -> create_string tl (str ^ hd ^ ", ")
  in create_string l ""

let rec stringify_i l = 
  let rec create_string l str = 
    match l with
    | [] -> str
    | [hd] -> str ^ (string_of_int hd) ^ "\n"
    | hd :: tl -> create_string tl (str ^ (string_of_int hd) ^ ", ")
  in create_string l ""

let rec stringify_f l = 
  let rec create_string l str = 
    match l with
    | [] -> str
    | [hd] -> str ^ (string_of_float hd) ^ "\n"
    | hd :: tl -> create_string tl (str ^ (string_of_float hd) ^ ", ")
  in create_string l ""

