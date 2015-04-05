open Core.Std
include Common
	  

(* Rather than using a typ option, I add a NoTyp constructor so that
 * the missing type information can be nested, e.g. IntTyp * (NoTyp -> BoolTyp)
*)
type typ =       BoolTyp
	 | IntTyp
         | StrTyp
	 | FunTyp of typ * typ
	 | PairTyp of typ * typ				
	 | ListTyp of typ
	 | Forall of variable * typ
	 | VarTyp of variable
         | NoTyp
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
  | Typecase of ((variable*typ) option) * typ *
                exp * exp * exp *
                variable * variable * exp *
                variable * variable * exp *
                variable * exp

  (* Function *)
  | App of exp * exp
  | Fun of variable * typ * exp		   
  | Rec of variable * variable * typ * typ * exp

  (* Type abstraction/application *)
  | TypLam of variable * exp
  | TypRec of variable * variable * typ * exp
  | TypApp of exp * typ
              deriving (Show)
