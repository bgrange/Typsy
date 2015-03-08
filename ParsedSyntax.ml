open SharedSyntax
open Core.Std

(* Match (e1, e2, hd, tl, e3) is a match statement with the following form:
 
   match e1 with 
     [] -> e2 
   | hd::tl -> e3 

   Closure (env, f, x, body) is a closure for a recursive function.
   The closure environment is env.  The recursive function is named f
   and x is the name of the parameter.  body is the body of the expression,
   and may contain f and x.

*)


(* Rather than using a typ option, I add a NoTyp constructor so that
 * the missing type information can be nested, e.g. IntTyp * (NoTyp -> BoolTyp)
 *)
type typ =       BoolTyp
	       | IntTyp
	       | FunTyp of typ * typ
	       | PairTyp of typ * typ				
	       | ListTyp of typ
	       | Forall of variable * typ
	       | VarTyp of variable
               | NoTyp
						      
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

  (* Function *)
  | App of exp * exp
  | Fun of variable * typ * exp		   
  | Rec of variable * variable * typ * typ * exp

  (* Type abstraction/application *)
  | TypLam of variable * exp
  | TypApp of exp * typ


(* Printing functions *)		      
		      
let max_prec = 10

let precedence e = 
  match e with 
    | Constant _ -> 0
    | Var _ -> 0
    | Op (_,Plus,_) -> 5
    | Op (_,Minus,_) -> 5
    | Op (_,Times,_) -> 3
    | Op (_,Div,_) -> 3
    | Op (_,Less,_) -> 7
    | Op (_,LessEq,_) -> 7
    | Let _ -> max_prec
    | If _ -> max_prec

    | Pair _ -> 0
    | Fst _ -> 2
    | Snd _ -> 2

    | EmptyList _ -> 0
    | Cons _ -> 8
    | Match _ -> max_prec

    | Rec _ -> max_prec
    | Fun _ -> max_prec		 
    | Closure _ -> max_prec
    | RecClosure _ -> max_prec		     
    | App _ ->  2

    | TypLam _ -> max_prec
    | TypClosure _ -> max_prec		    
    | TypApp _ -> 2

(*		    
let rec env2string env =
  let elem2string x v = x ^ "=" ^ exp2string max_prec v in
  let rec aux env =
    match env with
	[] -> ""
      | [(x,v)] -> elem2string x v
      | (x,v)::rest -> elem2string x v ^ ";" ^ aux rest 
  in
  "[" ^ aux env ^ "]"
 *)


let rec string_of_typ typ =
  match typ with
  | BoolTyp -> "bool"
  | IntTyp -> "int"
  | FunTyp (a,b) -> sprintf "(%s -> %s)" (string_of_typ a) (string_of_typ b)
  | PairTyp (a,b) -> sprintf "(%s * %s)" (string_of_typ a) (string_of_typ b)
  | ListTyp a -> sprintf "list %s" (string_of_typ a)
  | VarTyp v -> v
  | Forall (v,t) -> sprintf "forall %s, %s" v (string_of_typ t)
  | NoTyp -> "?"			    

		    
let rec exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant c -> string_of_const c
      | Op (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(string_of_op op)^" "^(exp2string prec e2)
      | Var x -> x
      | If (e1, e2, e3) -> 
        "if " ^ (exp2string max_prec e1) ^ 
        " then " ^ (exp2string max_prec e2) ^ 
        " else " ^ (exp2string p e3)
      | Let (x,e1,e2) -> "let "^x^" = "^(exp2string max_prec e1)^" in "^
          (exp2string prec e2)

      | Pair (e1, e2) -> 
	  "(" ^ (exp2string max_prec e1) ^ "," ^ (exp2string max_prec e2)  ^ ")"
      | Fst e1 ->  "fst " ^ (exp2string p e1)
      | Snd e1 ->  "snd " ^ (exp2string p e1)

      | EmptyList _ -> "[]"
      | Cons (e1,e2) -> (exp2string p e1) ^ "::" ^ (exp2string prec e2) 
      | Match (e1,e2,hd,tl,e3) -> 
	  "match " ^ (exp2string max_prec e1) ^ 
	    " with [] -> " ^ (exp2string max_prec e2) ^ 
            " | " ^ hd ^ "::" ^ tl ^ " -> " ^ (exp2string p e3)

      | Rec (f,x,t1,t2,body) -> sprintf "rec %s (%s:%s) : %s -> %s" f x
					(string_of_typ t1) (string_of_typ t2)
					(exp2string max_prec body)
      | Fun (x,t,body) -> sprintf "fun (%s:%s) -> %s" x
	                          (string_of_typ t)
	                          (exp2string max_prec body)		     		  
      | App (e1,e2) -> sprintf "%s %s" (exp2string p e1) (exp2string p e2)
      | TypLam (v,body) -> sprintf "tfun %s -> %s" v (exp2string p body)			       | TypApp (e',t) -> sprintf "%s [%s]" (exp2string p e') (string_of_typ t)	       
					       

  in 
    if p > prec then "(" ^ s ^ ")" else s

let string_of_exp e = exp2string max_prec e 
(*let string_of_env env = env2string env*)
