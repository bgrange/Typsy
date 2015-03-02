(*********************)
(* Dynamic ML Syntax *)
(*********************)

type variable = string 

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

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
  | Let of variable * exp * exp

  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

  (* Lists *)
  | EmptyList of typ
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp  

  (* Functions *)
  | Rec of variable * variable * typ * typ * exp
  | RecClosure of env * variable * variable * exp
  | Fun of variable * typ * exp
  | Closure of env * variable * exp			      
  | App of exp * exp

  (* Type abstraction/application *)
  | TypLam of variable * exp
  | TypApp of exp * typ
  | TypClosure of env * variable * exp

and env = (variable * exp) list * (variable * typ) list

let empty_env = ([],[])						   
						   
let rec lookup (l:(variable * 'a) list) (v:variable) : 'a option =
  match l with
  | [] -> None
  | (w,e)::l' ->
      if var_eq w v then Some e else lookup l' v

let lookup_exp (env:env) (v:variable) : exp option =
  lookup (fst env) v

let lookup_typ (env:env) (v:variable) : typ option =
  lookup (snd env) v

let update_exp (env:env) (v:variable) (e:exp) : env =
  ((v,e)::fst env,snd env)

let update_typ (env:env) (v:variable) (t:typ) : env =
  (fst env,(v,t)::snd env)
        
