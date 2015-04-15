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
  | TLet of variable * typ * exp
  | TRec of variable * variable * kind * typ * exp
  | TApp of exp * typ
              deriving (Show)


let walk
    (f:'st -> exp -> exp)
    (g:'st -> typ -> typ)
    (update:'st -> exp -> 'st)
    (s:'st)
    (e:exp) : exp =
  let rec aux s e =
    let s' = update s e in
    let e' =
      match e with
      | Var _ | Constant _ -> e
      | Op (e1,op,e2) -> Op (aux s' e1,op,aux s' e2)
      | If (e1,e2,e3) -> If (aux s' e1,aux s' e2, aux s' e3)
      | Pair (e1,e2) -> Pair (aux s' e1, aux s' e2)
      | Fst e1 -> Fst (aux s' e1)
      | Snd e1 -> Snd (aux s' e1)
      | EmptyList t -> EmptyList (g s t)
      | Cons (e1,e2) -> Cons (aux s' e1,aux s' e2)
      | Match (e1,e2,v1,v2,e3) -> Match (aux s' e1,aux s' e2,
                                         v1,v2,aux s' e3)
      | TCase (t1,t2,e1,e2,e3,e4,e5,e6) -> TCase (g s t1, g s t2,
                                                  aux s' e1, aux s' e2, aux s' e3,
                                                  aux s' e4, aux s' e5, aux s' e6)
      | App (e1,e2) -> App (aux s' e1, aux s' e2)
      | Fun (v,t,e1) -> Fun (v,g s t, aux s' e1)
      | Rec (v1,v2,t1,t2,e1) -> Rec (v1,v2,g s t1, g s t2, aux s' e1)
      | TFun (v,k,e1) -> TFun (v,k,aux s' e1)
      | TLet (v,t,e1) -> TLet (v,g s t, aux s' e1)
      | TRec (v1,v2,k,t,e1) -> TRec (v1,v2,k,g s t,aux s' e1)
      | TApp (e1,t) -> TApp (aux s' e1, g s t)
    in
    f s e'
  in
  aux s e

