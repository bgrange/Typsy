open Core.Std
include Common
	  

(* Rather than using a typ option, I add a NoneT constructor so that
 * the missing type information can be nested, e.g. IntT * (NoneT -> BoolT)
*)

type kind =
  | TypeK | ArrowK of kind * kind
  | NoneK
                      deriving (Show)


type typ =
    BoolT
  | IntT
  | StrT
  | VoidT
  | FunT of typ * typ
  | PairT of typ * typ				
  | ListT of typ
  | ForallT of variable * kind * typ
  | VarT of variable
  | TFunT of variable * kind * typ
  | TRecT of variable * variable * kind * kind * typ
  | TAppT of typ * typ
  | TCaseT of typ *
              typ * typ * typ *
              typ * typ * typ
  | NoneT
      deriving (Show)


type exp = 

  (* Basic *)
  | Var of variable   
  | Constant of constant
  | Op of exp * operator * exp
  | If of exp * exp * exp
  
  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

  (* Lists *)
  | EmptyList of typ
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp

  (* typecase of [d.d -> string] (list a) of ... *)
  | TCase of typ * typ *
                exp * exp * exp *
                exp * exp * exp

  (* Function *)
  | App of exp * exp
  | Fun of variable * typ * exp		   
  | Rec of variable * variable * typ * typ * exp

  (* Type abstraction/application *)
  | TFun of variable * kind * exp
  | TRec of variable * variable * kind * typ * exp
  | TApp of exp * typ
              deriving (Show)
