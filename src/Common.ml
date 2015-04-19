exception Impossible_error

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

(*
module SS = struct
  include Set.Make(String)
  module Show_t :
    Deriving_Show.Show with type a = t =
    Deriving_Show.Defaults(struct
      type a = t
      let format formatter _ =
        Format.fprintf formatter "<set>"
    end)
end
  
module SM = struct
  include Map.Make(String)
  module Show_t :
    Deriving_Show.Show with type 'x a = 'x t =
    Deriving_Show.Defaults(struct
      type 'x a = 'x t
      let format formatter _ =
        Format.fprintf formatter "<map>"
    end)
end *)
