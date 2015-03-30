open Common
open Type

type tenv = typ SM.t
    
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
  | Typecase of (variable*typ) * typ *
                exp * exp *
                variable * variable * exp *
                variable * variable * exp *
                variable * exp

  (* Function *)
  | App of exp * exp
  | Fun of variable * exp * SS.t * SS.t		   
  | Rec of variable * exp * SS.t * SS.t

  (* Type abstraction/application *)
  | TypLam of variable * exp * SS.t * SS.t
  | TypApp of exp * typ

  (* Closures *)		       
  | Closure of env * tenv * variable * exp
  | RecClosure of env * tenv * variable * variable * exp
and env = exp SM.t


