type variable = string
  deriving (Show)

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool 
  deriving (Show)

type operator = Plus | Minus | Times | Div | Less | LessEq 
  deriving (Show)

module SS = Set.Make(String)
module SM = Map.Make(String)

						      
