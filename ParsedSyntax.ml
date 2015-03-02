type variable = string
type constant = Int of int | Bool of bool 
type operator = Plus | Minus | Times | Div | Less | LessEq 

(* Match (e1, e2, hd, tl, e3) is a match statement with the following form:
 
   match e1 with 
     [] -> e2 
   | hd::tl -> e3 

   Closure (env, f, x, body) is a closure for a recursive function.
   The closure environment is env.  The recursive function is named f
   and x is the name of the parameter.  body is the body of the expression,
   and may contain f and x.

*)

type typ =   BoolTyp
	   | IntTyp
	   | FunTyp of typ * typ
	   | PairTyp of typ * typ				
	   | ListTyp of typ
	   | Forall of variable * typ
	   | VarTyp of variable
						      
type exp = 

  (* Basic *)
  | Var of variable   
  | Constant of constant
  | Op of exp * operator * exp
  | If of exp * exp * exp
  | Let of ((variable * typ option) list) * (typ option) * exp * exp

  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

  (* Lists *)
  | EmptyList of typ option
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp  

  (* Function *)
  | App of exp * exp
  | Fun of ((variable * typ option) list) * exp		   

  (* Type abstraction/application *)
  | TypLam of (variable list) * exp
  | TypApp of exp * typ option
