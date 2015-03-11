open Environment     
open SharedSyntax
open Type

module TS = TypedSyntax
module SS = Set.Make(String)   
type tenv = typ Env.t
    
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
  | EmptyList
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp  

  (* Function *)
  | App of exp * exp
  | Fun of variable * exp * SS.t * SS.t		   
  | Rec of variable * variable * exp * SS.t * SS.t

  (* Type abstraction/application *)
  | TypLam of variable * exp * SS.t * SS.t
  | TypApp of exp * typ

  (* Closures *)		       
  | Closure of env * tenv * variable * exp
  | RecClosure of env * tenv * variable * variable * exp
and env = exp Env.t


