type variable = string 

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool 

type operator = Plus | Minus | Times | Div | Less | LessEq 

type typopt =    BoolTyp
	       | IntTyp
	       | FunTyp of typopt * typopt
	       | PairTyp of typopt * typopt				
	       | ListTyp of typopt
	       | Forall of variable * typopt
	       | VarTyp of variable
               | NoTyp
       

type typ =   BoolTyp
	   | IntTyp
	   | FunTyp of typ * typ
	   | PairTyp of typ * typ				
	   | ListTyp of typ
	   | Forall of variable * typ
	   | VarTyp of variable
						      
type 't exp = 

  (* Basic *)
  | Var of variable   
  | Constant of constant
  | Op of 't exp * operator * 't exp
  | If of 't exp * 't exp * 't exp

  (* Pairs *)
  | Pair of 't exp * 't exp
  | Fst of 't exp
  | Snd of 't exp

  (* Lists *)
  | EmptyList of 't
  | Cons of 't exp * 't exp  
  | Match of 't exp * 't exp * variable * variable * 't exp  

  (* Functions *)
  | Rec of variable * variable * 't * 't * 't exp
  | RecClosure of env * variable * variable * 't exp
  | Fun of variable * 't * 't exp
  | Closure of env * variable * 't exp			      
  | App of 't exp * 't exp

  (* Type abstraction/application *)
  | TypLam of variable * 't exp
  | TypApp of 't exp * 't
  | TypClosure of env * variable * 't exp
 and typed_exp = typ exp
 and parsed_exp = typopt exp		     
and env = (variable * typed_exp) list * (variable * typ) list

let empty_env = ([],[])						   
						   
let rec lookup (l:(variable * 'a) list) (v:variable) : 'a option =
  match l with
  | [] -> None
  | (w,e)::l' ->
      if var_eq w v then Some e else lookup l' v

let lookup_exp (env:env) (v:variable) : typed_exp option =
  lookup (fst env) v

let lookup_typ (env:env) (v:variable) : typ option =
  lookup (snd env) v

let update_exp (env:env) (v:variable) (e:exp) : env =
  ((v,e)::fst env,snd env)

let update_typ (env:env) (v:variable) (t:typ) : env =
  (fst env,(v,t)::snd env)


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


let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b


let string_of_op op = 
  match op with 
    | Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*" 
    | Div -> "/" 
    | Less -> "<" 
    | LessEq -> "<=" 						      

		    
let rec string_of_typ t =
  match t with
  | BoolTyp -> "bool"
  | IntTyp -> "int"
  | FunTyp (a,b) -> sprintf "(%s -> %s)" (string_of_typ a) (string_of_typ b)
  | PairTyp (a,b) -> sprintf "(%s * %s)" (string_of_typ a) (string_of_typ b)
  | ListTyp a -> sprintf "list %s" (string_of_typ a)
  | VarTyp v -> v
  | Forall (v,t) -> sprintf "forall %s, %s" v (string_of_typ t)

let rec string_of_typopt t =
  match t with
  | BoolTyp -> "bool"
  | IntTyp -> "int"
  | FunTyp (a,b) -> sprintf "(%s -> %s)" (string_of_typ a) (string_of_typ b)
  | PairTyp (a,b) -> sprintf "(%s * %s)" (string_of_typ a) (string_of_typ b)
  | ListTyp a -> sprintf "list %s" (string_of_typ a)
  | VarTyp v -> v
  | Forall (v,t) -> sprintf "forall %s, %s" v (string_of_typ t)
  | NoTyp -> "?"			    

		    
let rec exp2string prec (e:'t exp) (sot:'t -> string) = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant c -> string_of_const c
      | Op (e1,op,e2) -> 
          (exp2string p e1 sot) ^ " "^(string_of_op op)^" "^(exp2string prec e2 sot)
      | Var x -> x
      | If (e1, e2, e3) -> 
        "if " ^ (exp2string max_prec e1 sot) ^ 
        " then " ^ (exp2string max_prec e2 sot) ^ 
        " else " ^ (exp2string p e3 sot)
      | Pair (e1, e2) -> 
	  "(" ^ (exp2string max_prec e1 sot) ^ "," ^ (exp2string max_prec e2 sot)  ^ ")"
      | Fst e1 ->  "fst " ^ (exp2string p e1 sot)
      | Snd e1 ->  "snd " ^ (exp2string p e1 sot)

      | EmptyList _ -> "[]"
      | Cons (e1,e2) -> (exp2string p e1 sot) ^ "::" ^ (exp2string prec e2 sot) 
      | Match (e1,e2,hd,tl,e3) -> 
	  "match " ^ (exp2string max_prec e1 sot) ^ 
	    " with [] -> " ^ (exp2string max_prec e2 sot) ^ 
            " | " ^ hd ^ "::" ^ tl ^ " -> " ^ (exp2string p e3 sot)

      | Rec (f,x,t1,t2,body) -> sprintf "rec %s (%s:%s) : %s -> %s" f x
					(sot t1) (sot t2)
					(exp2string max_prec body sot)
      | Fun (x,t,body) -> sprintf "fun (%s:%s) -> %s" x
	                          (sot t)
	                          (exp2string max_prec body)		     
      | Closure _ -> "<closure>"
      | RecClosure _ -> "<rec_closure>"
      | TypClosure _ -> "<typ_closure>"			  
      | App (e1,e2) -> sprintf "%s %s" (exp2string p e1 sot) (exp2string p e2 sot)
      | TypLam (v,body) -> sprintf "tfun %s -> %s" v (exp2string p body sot)			       | TypApp (e',t) -> sprintf "%s [%s]" (exp2string p e' sot) (sot t)	       
					       

  in 
    if p > prec then "(" ^ s ^ ")" else s

let string_of_typed_exp e = exp2string max_prec e string_of_typ
let string_of_parsed_exp e = exp2string max_prec e string_of_typopt				       
(*let string_of_env env = env2string env*)
