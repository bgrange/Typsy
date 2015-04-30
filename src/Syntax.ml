open Common

exception Closure_error
exception Missing_type
exception Missing_kind

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
  | TAppT of typ * typ
  | TRecT of typ *
              typ * typ * typ *
              typ * typ * typ * typ
  | NoneT
      deriving (Show)

type exp = 
  (* Basic *)
  | Var of variable   
  | Constant of constant
  | Unop of unop * exp
  | Binop of exp * binop * exp
  | If of exp * exp * exp
  
  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

  (* Lists *)
  | EmptyList of typ
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp
             
  | TCase of typ * typ *
                exp * exp * exp *
                exp * exp * exp * exp

  (* Function *)
  | App of exp * exp
  | Fun of variable * typ * exp		   
  | Rec of variable * variable * typ * typ * exp

  (* Type abstraction/application *)
  | TFun of variable * kind * exp
  | TRec of variable * variable * kind * typ * exp
  | TApp of exp * typ

              (* Closures *)		       
  | Closure of env * tenv * variable * exp
  | RecClosure of env * tenv * variable * variable * exp
and env = exp SM.t
and tenv = typ SM.t
