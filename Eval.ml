(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open SharedSyntax
open EvalSyntax
open Type  
open Environment       
       
exception BadList of exp
exception UnboundVariable of variable 
exception BadApplication of exp 
exception BadIf of exp 
exception BadMatch of exp 
exception BadOp of exp * operator * exp 
exception BadPair of exp


module SS = Set.Make(String) ;;

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
    | EmptyList -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | _ -> false
      
let prune_env (env:'a Env.t) (fvars:SS.t) (e:exp) : 'a Env.t =
  Env.filter (fun v -> SS.mem v fvars) env

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:exp Env.t) (tenv:typ Env.t)
	      (eval_loop:exp Env.t -> typ Env.t -> exp -> exp) (e:exp) : exp =
  match e with
    | Var x -> 
      (match Env.lookup env x with 
  	  None -> raise (UnboundVariable x)
	| Some v -> v)
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
             let env' = Env.update env x_hd v_hd in
             let env'' = Env.update env' x_tl v_tl in
             eval_loop env'' tenv e3
         | _ -> raise (BadList v1))
    | Rec (f,arg,body,fvars,ftvars) ->
       RecClosure (prune_env env fvars body,
		   prune_env tenv ftvars body,
		   f,arg,body)
    | Fun (arg,body,fvars,ftvars) ->
       Closure (prune_env env fvars body,
		prune_env tenv ftvars body,
		arg,body)
    | Closure _ | RecClosure _  -> e
    | App (e1,e2) ->
        let v1 = eval_loop env tenv e1 in
        let v2 = eval_loop env tenv e2 in
        (match v1 with
         | RecClosure (env_cl,tenv_cl,f,arg,body) ->
             let env_cl' = Env.update env_cl arg v2 in
             let env_cl'' = Env.update env_cl' f v1 in
             eval_loop env_cl'' tenv body
         | Closure (env_cl,tenv_cl,arg,body) ->
	     let env_cl' = Env.update env_cl arg v2 in
             eval_loop env_cl' tenv body             
         | _ -> raise (BadApplication e))
    | TypLam (v,e',fvars,ftvars) ->
        Closure (prune_env env fvars e',
                 prune_env tenv ftvars e', v, e')			       
    | TypApp (e',t) ->
       let ve' = eval_loop env tenv e' in
       (match ve' with
	| Closure (env_cl,tenv_cl,arg,body) ->
	   let tenv_cl' = Env.update tenv_cl arg t in
	   eval_loop env_cl tenv_cl' body
	| _ -> raise (BadApplication e))
;;

(* evaluate closed, top-level expression e *)

let eval (e:EvalSyntax.exp) =
  let rec loop env tenv e = eval_body env tenv loop e in
  loop Env.empty Env.empty e


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
  loop Env.empty Env.empty e
