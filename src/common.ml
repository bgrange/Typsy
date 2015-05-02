exception Impossible_error

type filename = string

type variable = string
  deriving (Show)

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool | Str of string | Emp
  deriving (Show)

type binop = Plus | Minus | Times | Div | Mod | Less | LessEq | Gt | GtEq
              | CharAt | Concat | Eq | And | Or | StrEq | BoolEq
                deriving (Show)
type unop = StrLen
  deriving (Show)

module SS = Set.Make(String)
module SM = Map.Make(String)

