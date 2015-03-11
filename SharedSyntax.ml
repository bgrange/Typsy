type variable = string
  deriving (Show)

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool 
  deriving (Show)

type operator = Plus | Minus | Times | Div | Less | LessEq 
  deriving (Show)

let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b


let string_of_op op = 
  match op with 
    | Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*" 
    | Div -> "/" 
    | Less -> "<" 
    | LessEq -> "<=" 

						      
