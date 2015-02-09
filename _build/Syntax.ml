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
  | EmptyList
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp  

  (* Recursive functions *)
  | Rec of variable * variable * exp
  | Closure of env * variable * variable * exp 
  | App of exp * exp

and env = (variable * exp) list

(*****************************)
(* Manipulating environments *)
(*****************************)
 
(* empty environment *)
let empty_env : env = []

(* lookup_env env x == Some v 
 *   where (x,v) is the most recently added pair (x,v) containing x
 * lookup_env env x == None 
 *   if x does not appear in env *)
let rec lookup_env (env:env) (x:variable) : exp option =
  match env with
    [] -> None
  | (y,v)::env' -> if var_eq x y then Some v else lookup_env env' x 
;;

(* update env x v returns a new env containing the pair (x,v) *)
let rec update_env (env:env) (x:variable) (v:exp) : env =
  match env with
    [] -> [(x,v)]
  | hd::tl ->
      let (y,_) = hd in
      if x = y then (x,v)::tl
      else hd::(update_env tl x v)
;;
