(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open TypedSyntax
open Printing
open Util
       
exception BadList of exp
exception UnboundVariable of variable 
exception BadApplication of exp 
exception BadIf of exp 
exception BadMatch of exp 
exception BadOp of exp * operator * exp 
exception BadPair of exp
		       
(* Defines the subset of expressions considered values
   Notice that closures are values but the rec form is not -- this is
   slightly different from the way values are defined in the 
   substitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
 *)

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
       | Constant (Int i), Less, Constant (Int j) -> 
         Constant (Bool (i<j))
       | Constant (Int i), LessEq, Constant (Int j) -> 
         Constant (Bool (i<=j))
       | _, _, _ -> raise (BadOp (v1,op,v2))
			  
let rec is_value (e:exp) : bool = 
  match e with
      Constant _ -> true  
    | Pair (e1, e2) -> is_value e1 && is_value e2
    | EmptyList _ -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | TypClosure _ -> true		     
    | _ -> false

let prune_env (env:env) (e:exp) : env =
  let normal_frees = free_vars e in
  let tvar_frees = free_tvars e in
  (List.filter (fun (v,_) -> SS.mem v normal_frees) (fst env),
   List.filter (fun (v,_) -> SS.mem v tvar_frees) (snd env))
	     
(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp =
  match e with
    | Var x -> 
      (match lookup_exp env x with 
  	  None -> raise (UnboundVariable x)
	| Some v -> v)
    | Constant _ -> e
    | Op (e1,op,e2) ->
        let v1 = eval_loop env e1 in
        let v2 = eval_loop env e2 in
        apply_op v1 op v2
    | If (e1,e2,e3) -> 
        (match eval_loop env e1 with 
           | Constant (Bool true) -> eval_loop env e2
           | Constant (Bool false) -> eval_loop env e3
           | v1 -> raise (BadIf v1))
    | Let (x,e1,e2) -> eval_loop (update_exp env x e1) e2
    | Pair (e1,e2) -> Pair(eval_loop env e1, eval_loop env e2)
    | Fst p -> 
        (match eval_loop env p with
           Pair(first, _) -> first
         | _ -> raise (BadPair p))
    | Snd p -> 
        (match eval_loop env p with
           Pair(_, second) -> second
         | _ -> raise (BadPair p))
    | EmptyList _ -> e
    | Cons (e1,e2) ->
        let hd = eval_loop env e1 in
        let tl = eval_loop env e2 in
        (match tl with
           EmptyList _ | Cons _ -> Cons (hd,tl)
         | _ -> raise (BadList tl))
    | Match (e1, e2, x_hd, x_tl, e3) ->
        let v1 = eval_loop env e1 in
        (match v1 with
           EmptyList _ -> eval_loop env e2
         | Cons (v_hd, v_tl) ->
             let env' = update_exp env x_hd v_hd in
             let env'' = update_exp env' x_tl v_tl in
             eval_loop env'' e3
         | _ -> raise (BadList v1))
    | Rec (f,arg,_,_,body) ->
       RecClosure (prune_env env body,f,arg,body)
    | Fun (arg,_,body) ->
       Closure (prune_env env body,arg,body)
    | Closure _ | RecClosure _ | TypClosure _ -> e
    | App (e1,e2) ->
        let v1 = eval_loop env e1 in
        let v2 = eval_loop env e2 in
        (match v1 with
         | RecClosure (env_cl,f,arg,body) ->
             let env_cl' = update_exp env_cl arg v2 in
             let env_cl'' = update_exp env_cl' f v1 in
             eval_loop env_cl'' body
         | Closure (env_cl,arg,body) ->
	     let env_cl' = update_exp env_cl arg v2 in
             eval_loop env_cl' body             
         | _ -> raise (BadApplication e))
    | TypLam (v,e') -> TypClosure (prune_env env e', v, e')
    | TypApp (e',t) ->
       let ve' = eval_loop env e' in
       (match ve' with
	| TypClosure (env_cl,arg,body) ->
	   let env_cl' = update_typ env_cl arg t in
	   eval_loop env_cl' body
	| _ -> raise (BadApplication e))
;;

(* evaluate closed, top-level expression e *)

let eval e =
  let rec loop env e = eval_body env loop e in
  loop empty_env e


(* print out subexpression after each step of evaluation *)
let debug_eval e = 
  let rec loop env e =
    if is_value e then e  (* don't print values *)
    else 
      begin
	Printf.printf "Evaluating %s\n" (string_of_exp e); 
	let v = eval_body env loop e in 
	Printf.printf 
	  "%s evaluated to %s\n" (string_of_exp e) (string_of_exp v); 
	v
      end
  in
  loop empty_env e
