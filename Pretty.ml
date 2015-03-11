open EvalSyntax
open SharedSyntax
open Type

let rec string_of_typ typ =
  match typ with
  | BoolTyp -> "bool"
  | IntTyp -> "int"
  | FunTyp (a,b) -> Printf.sprintf "(%s -> %s)" (string_of_typ a) (string_of_typ b)
  | PairTyp (a,b) -> Printf.sprintf "(%s * %s)" (string_of_typ a) (string_of_typ b)
  | ListTyp a -> Printf.sprintf "list %s" (string_of_typ a)
  | VarTyp v -> v
  | Forall (v,t) -> Printf.sprintf "forall %s, %s" v (string_of_typ t)

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
    | If _ -> max_prec

    | Pair _ -> 0
    | Fst _ -> 2
    | Snd _ -> 2

    | EmptyList -> 0
    | Cons _ -> 8
    | Match _ -> max_prec

    | Rec _ -> max_prec
    | Fun _ -> max_prec		 
    | Closure _ -> max_prec
    | RecClosure _ -> max_prec		     
    | App _ ->  2

    | TypLam _ -> max_prec
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
      | Pair (e1, e2) -> 
	  "(" ^ (exp2string max_prec e1) ^ "," ^ (exp2string max_prec e2)  ^ ")"
      | Fst e1 ->  "fst " ^ (exp2string p e1)
      | Snd e1 ->  "snd " ^ (exp2string p e1)

      | EmptyList -> "[]"
      | Cons (e1,e2) -> (exp2string p e1) ^ "::" ^ (exp2string prec e2) 
      | Match (e1,e2,hd,tl,e3) -> 
	  "match " ^ (exp2string max_prec e1) ^ 
	    " with [] -> " ^ (exp2string max_prec e2) ^ 
            " | " ^ hd ^ "::" ^ tl ^ " -> " ^ (exp2string p e3)

      | Rec (f,x,body,_,_) -> Printf.sprintf "rec %s %s -> %s" f x (exp2string max_prec body)
      | Fun (x,body,_,_) -> Printf.sprintf "fun %s -> %s" x
	                          (exp2string max_prec body)		     		  
      | App (e1,e2) -> Printf.sprintf "%s %s" (exp2string p e1) (exp2string p e2)
      | TypLam (v,body,_,_) -> Printf.sprintf "tfun %s -> %s" v (exp2string p body)			       | TypApp (e',t) -> Printf.sprintf "%s [%s]" (exp2string p e') (string_of_typ t)
      | Closure _ | RecClosure _ -> "<closure>"		      
  in 
    if p > prec then "(" ^ s ^ ")" else s

let string_of_exp e = exp2string max_prec e 
(*let string_of_env env = env2string env*)
