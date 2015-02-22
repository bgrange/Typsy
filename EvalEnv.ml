(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Syntax
open Printing
open EvalUtil

exception BadList of exp

(* Defines the subset of expressions considered values
   Notice that closures are values but the rec form is not -- this is
   slightly different from the way values are defined in the 
   substitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
  *)
let rec is_value (e:exp) : bool = 
  match e with
      Constant _ -> true  
    | Pair (e1, e2) -> is_value e1 && is_value e2
    | EmptyList _ -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | _ -> false

let free_vars (e:exp) : variable list =
  let add_no_dups xs x =
    if List.exists (var_eq x) xs then xs
    else x::xs
  in
  let union xs ys = List.fold_left add_no_dups xs ys in
  let rec aux e bound =
    match e with
      Var x ->
        if List.exists (var_eq x) bound then [] 
        else [x]
    | Constant _ -> []
    | Op (e1,op,e2) ->
        union (aux e1 bound)
              (aux e2 bound)
    | If (e1,e2,e3) ->
        union (union (aux e1 bound)
                     (aux e2 bound))
                     (aux e3 bound)
    | Let (x,e1,e2) ->
        let free_e1 = aux e1 bound in
        union free_e1 (aux e2 (x::bound))
    | Pair (e1,e2) ->
        union (aux e1 bound)
              (aux e2 bound)
    | Fst p -> aux p bound
    | Snd p -> aux p bound
    | EmptyList _ -> [] 
    | Cons (hd,tl) -> 
        union (aux hd bound)
              (aux tl bound)
    | Match (e1,e2,x_hd,x_tl,e3) ->
        union (union (aux e1 bound)
                     (aux e2 bound))
                     (aux e3 (x_hd::x_tl::bound))
    | Rec (name,arg,_,_,body) ->
        aux body (name::arg::bound)
    | Closure _ -> [] 
    | App (e1,e2) ->  
        union (aux e1 bound)
              (aux e2 bound)
  in aux e []
;;

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  match e with
    | Var x -> 
      (match lookup_env env x with 
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
    | Let (x,e1,e2) -> eval_loop (update_env env x e1) e2
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
             let env' = update_env env x_hd v_hd in
             let env'' = update_env env' x_tl v_tl in
             eval_loop env'' e3
         | _ -> raise (BadList v1))
    | Rec (f,arg,_,_,body) ->
        let frees = free_vars e in
        let env' = List.filter
                     (fun (x,_) -> 
                        List.exists (var_eq x) frees) env
        in Closure (env',f,arg,body)
    | Closure _ -> e
    | App (e1,e2) ->
        let v1 = eval_loop env e1 in
        let v2 = eval_loop env e2 in
        (match v1 with
           Closure (env_cl,f,arg,body) ->
             let env_cl' = update_env env_cl arg v2 in
             let env_cl'' = update_env env_cl' f v1 in
             eval_loop env_cl'' body
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
