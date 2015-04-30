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
  | ForallT of (variable * kind) list * typ
  | VarT of variable
  | TFunT of (variable * kind) list * typ
  | TAppT of typ * typ
  | TRecT of variable * typ * kind * (typ * typ) list
  | NoneT
      deriving (Show)

type typorkind = T of typ | K of kind
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

  (* typecase of [d.d -> string] (list a) of ... *)
  | TCase of typ * typ * (typ * exp) list
            
  (* Function *)
  | App of exp * exp
  | Fun of (variable * typorkind) list * exp		   
  | Rec of variable * (variable * typorkind) list * typ * exp
           
  | Let of variable * (variable * typorkind) list * typ * exp * exp
  | LetRec of variable * (variable * typorkind) list * typ * exp * exp
  | TLet of variable * (variable * kind) list * typ * exp

  (* Type abstraction/application *)
(*  | TFun of (variable * kind) list * exp
    | TRec of variable * (variable * kind) list * typ * exp *)
  | TApp of exp * typ
              deriving (Show)
