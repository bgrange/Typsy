(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Common
open EvalSyntax
open Type  
open Util
       
exception BadList of exp
exception UnboundVariable of variable 
exception BadApplication of exp 
exception BadIf of exp 
exception BadMatch of exp 
exception BadOp of exp * operator * exp 
exception BadPair of exp
exception BadTypecase of exp
    

let apply_op (v1:exp) (op:operator) (v2:exp) : exp = 
     match v1, op, v2 with 
       | Constant (Int i), Plus, Constant (Int j) -> 
         Constant (Int (i+j))
       | Constant (Int i), Minus, Constant (Int j) -> 
         Constant (Int (i-j))
       | Constant (Int i), Times, Constant (Int j) -> 
         Constant (Int (i*j))
       | Constant (Int i), Div, Constant (Int j) -> 
         Constant (Int (i/j))
       | Constant (Int i), Mod, Constant (Int j) ->
         Constant (Int (i mod j))
       | Constant (Int i), Less, Constant (Int j) -> 
         Constant (Bool (i<j))
       | Constant (Int i), LessEq, Constant (Int j) -> 
         Constant (Bool (i<=j))
       | Constant (Str s1), Concat, Constant (Str s2) ->
         Constant (Str (s1 ^ s2))
       | Constant (Int i), Eq, Constant (Int j) -> 
         Constant (Bool (i = j))
       | Constant (Bool b1), And, Constant (Bool b2) -> 
         Constant (Bool (b1 && b2))
       | Constant (Bool b1), Or, Constant (Bool b2) -> 
         Constant (Bool (b1 || b2))           
       | _, _, _ -> raise (BadOp (v1,op,v2))

let rec is_value (e:exp) : bool = 
  match e with
      Constant _ -> true  
    | Pair (e1, e2) -> is_value e1 && is_value e2
    | EmptyList -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | _ -> false
      
let prune_env (env:'a SM.t) (fvars:SS.t) : 'a SM.t =
  SM.filter (fun v _ -> SS.mem v fvars) env

(* substitute for the free variables in t *)
(* this is virtually identical to Util.sub_in_typ
   maybe something can be factored out *)    
let eval_typ (tenv:typ SM.t) (t:typ) : typ =
  let rec aux t b =
    match t with
    | BoolTyp | IntTyp | StrTyp -> t
    | FunTyp (t1,t2) -> FunTyp (aux t1 b, aux t2 b)
    | PairTyp (t1,t2) -> PairTyp (aux t1 b, aux t2 b)
    | ListTyp t1 -> ListTyp (aux t1 b)
    | VarTyp v ->
      if SS.mem v b then t else
        (try SM.find v tenv
         with Not_found -> raise (UnboundVariable v))
    | Forall (v,u) -> Forall (v,aux u b)
  in
  aux t SS.empty

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:exp SM.t) (tenv:typ SM.t)
    (eval_loop:exp SM.t -> typ SM.t -> exp -> exp) (e:exp) : exp =
(*  let bindings = SM.bindings env in
  let tbindings = SM.bindings tenv in*)
  match e with
    | Var x -> 
      (try SM.find x env
       with Not_found -> raise (UnboundVariable x))

    | Constant _ -> e
    | Op (e1,op,e2) ->
        let v1 = eval_loop env tenv e1 in
        let v2 = eval_loop env tenv e2 in
        apply_op v1 op v2
    | If (e1,e2,e3) -> 
        (match eval_loop env tenv e1 with 
           | Constant (Bool true) -> eval_loop env tenv e2
           | Constant (Bool false) -> eval_loop env tenv e3
           | v1 -> raise (BadIf v1))
    | Pair (e1,e2) -> Pair(eval_loop env tenv e1, eval_loop env tenv e2)
    | Fst p -> 
        (match eval_loop env tenv p with
           Pair(first, _) -> first
         | _ -> raise (BadPair p))
    | Snd p -> 
        (match eval_loop env tenv p with
           Pair(_, second) -> second
         | _ -> raise (BadPair p))
    | EmptyList -> e
    | Cons (e1,e2) ->
        let hd = eval_loop env tenv e1 in
        let tl = eval_loop env tenv e2 in
        (match tl with
           EmptyList | Cons _ -> Cons (hd,tl)
         | _ -> raise (BadList tl))
    | Match (e1, e2, x_hd, x_tl, e3) ->
        let v1 = eval_loop env tenv e1 in
        (match v1 with
           EmptyList -> eval_loop env tenv e2
         | Cons (v_hd, v_tl) ->
             let env' = SM.add x_hd v_hd env in
             let env'' = SM.add x_tl v_tl env' in
             eval_loop env'' tenv e3
         | _ -> raise (BadList v1))
    | Rec (f,arg,body,fvars,ftvars) ->
       RecClosure (prune_env env fvars,
		   prune_env tenv ftvars,
		   f,arg,body) 
    | Fun (arg,body,fvars,ftvars) ->
       Closure (prune_env env fvars,
		prune_env tenv ftvars,
	 arg,body)
    | Closure _ | RecClosure _  -> e
    | App (e1,e2) ->
        let v1 = eval_loop env tenv e1 in
        let v2 = eval_loop env tenv e2 in
        (match v1 with
         | RecClosure (env_cl,tenv_cl,f,arg,body) ->
             let env_cl' = SM.add arg v2 env_cl in
             let env_cl'' = SM.add f v1 env_cl' in
             eval_loop env_cl'' tenv_cl body
         | Closure (env_cl,tenv_cl,arg,body) ->
	     let env_cl' = SM.add arg v2 env_cl in
             eval_loop env_cl' tenv_cl body             
         | _ -> raise (BadApplication e))
    | TypLam (v,e',fvars,ftvars) ->
        Closure (prune_env env fvars,
                 prune_env tenv ftvars, v, e')
    | TypRec (f,arg,body,fvars,ftvars) ->
      RecClosure (prune_env env fvars,
                  prune_env tenv ftvars,
                  f,arg,body)
    | TypApp (e',t) ->
      let ve' = eval_loop env tenv e' in
      let closed_t = eval_typ tenv t in
       (match ve' with
	| Closure (env_cl,tenv_cl,arg,body) ->
          let tenv_cl' = SM.add arg closed_t tenv_cl in
	  eval_loop env_cl tenv_cl' body
        | RecClosure (env_cl,tenv_cl,f,arg,body) ->
          let tenv_cl' = SM.add arg closed_t tenv_cl in
          let env_cl' = SM.add f ve' env_cl in
          eval_loop env_cl' tenv_cl' body
        | _ -> raise (BadApplication e))
    | Typecase ((v,t),alpha,
                eint,ebool,estr,
                a,b,efun,
                c,d,epair,
                f,elist) ->
      let closed_alpha = eval_typ tenv alpha in
      (match closed_alpha with
       | BoolTyp -> eval_loop env tenv ebool
       | IntTyp -> eval_loop env tenv eint
       | StrTyp -> eval_loop env tenv estr
       | FunTyp (t1,t2) ->
         let new_tenv = SM.add b t2 (SM.add a t1 tenv) in
         eval_loop env new_tenv efun
       | PairTyp (t1,t2) ->
         let new_tenv = SM.add d t2 (SM.add c t1 tenv) in
         eval_loop env new_tenv epair
       | ListTyp t' ->
         let new_tenv = SM.add f t' tenv in
         eval_loop env new_tenv elist
       | _ -> raise (BadTypecase e))
;;

(* evaluate closed, top-level expression e *)

let eval (e:EvalSyntax.exp) =
  let rec loop env tenv e = eval_body env tenv loop e in
  loop SM.empty SM.empty e


(* print out subexpression after each step of evaluation *)
let debug_eval e = 
  let rec loop env tenv e =
    if is_value e then e  (* don't print values *)
    else 
      begin
	Printf.printf "Evaluating %s\n" (Pretty.string_of_exp e); 
	let v = eval_body env tenv loop e in 
	Printf.printf 
	  "%s evaluated to %s\n" (Pretty.string_of_exp e) (Pretty.string_of_exp v); 
	v
      end
  in
  loop SM.empty SM.empty e
