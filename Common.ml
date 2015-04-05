type variable = string
  deriving (Show)

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool | Str of string
  deriving (Show)

type operator = Plus | Minus | Times | Div | Mod | Less | LessEq | Concat |
                Eq | And | Or
  deriving (Show)

module SS = Set.Make(String)
module SM = Map.Make(String)

						      
