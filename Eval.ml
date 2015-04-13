(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Common
open TypedSyntax
open Util
       
exception BadList of exp
exception UnboundVariable of variable 
exception BadApplication of exp 
exception BadIf of exp 
exception BadMatch of exp 
exception BadOp of exp * operator * exp 
exception BadPair of exp
exception BadTCase of exp
    

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
    | EmptyList _ -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | RecClosure _ -> true
    | _ -> false
      
let prune_env (env:env) (e:exp) : env =
  let frees = free_vars e in
  SM.filter (fun v _ -> SS.mem v frees) env

let prune_tenv (tenv:tenv) (e:exp) : tenv = 
  let frees = free_tvars e in
  SM.filter (fun v _ -> SS.mem v frees) tenv

(* substitute for the free variables in t *)
(* this is virtually identical to Util.sub_in_typ
   maybe something can be factored out *)    
let sub_all (tenv:tenv) (t:typ) : typ =
  let rec aux t b =
    match t with
    | BoolT | IntT | StrT | VoidT -> t
    | FunT (t1,t2) -> FunT (aux t1 b, aux t2 b)
    | PairT (t1,t2) -> PairT (aux t1 b, aux t2 b)
    | ListT t1 -> ListT (aux t1 b)
    | VarT v ->
      if SS.mem v b then t else
        (try SM.find v tenv
         with Not_found -> raise (UnboundVariable v))
    | ForallT (v,k,u) -> ForallT (v,k,aux u (SS.add v b))
    | TFunT (v,k,u) -> TFunT (v,k,aux u (SS.add v b))
    | TAppT (t1,t2) -> TAppT (aux t1 b, aux t2 b)
    | TRecT (f,v,k1,k2,u) -> TRecT (f,v,k1,k2,
                                    aux u (SS.add f (SS.add v b)))
    | TCaseT (alpha,t1,t2,t3,t4,t5,t6) ->
      TCaseT (aux alpha b,
              aux t1 b, aux t2 b,
              aux t3 b, aux t4 b,
              aux t5 b, aux t6 b)
  in
  aux t SS.empty

let eval_typ tenv t : typ =
  Util.normalize_type (sub_all tenv t)

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (tenv:tenv)
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
    | EmptyList _ -> e
    | Cons (e1,e2) ->
        let hd = eval_loop env tenv e1 in
        let tl = eval_loop env tenv e2 in
        (match tl with
           EmptyList _ | Cons _ -> Cons (hd,tl)
         | _ -> raise (BadList tl))
    | Match (e1, e2, x_hd, x_tl, e3) ->
        let v1 = eval_loop env tenv e1 in
        (match v1 with
           EmptyList _ -> eval_loop env tenv e2
         | Cons (v_hd, v_tl) ->
             let env' = SM.add x_hd v_hd env in
             let env'' = SM.add x_tl v_tl env' in
             eval_loop env'' tenv e3
         | _ -> raise (BadList v1))
    | Rec (f,arg,_,_,body) ->
       RecClosure (SM.remove f (SM.remove arg (prune_env env body)),
		   prune_tenv tenv body,
		   f,arg,body) 
    | Fun (arg,_,body) ->
       Closure (SM.remove arg (prune_env env body),
		prune_tenv tenv body,
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
    | TFun (v,_,e') ->
        Closure (prune_env env e',
                 SM.remove v (prune_tenv tenv e'),
                 v, e')
    | TRec (f,arg,_,_,body) ->
      RecClosure (SM.remove f (prune_env env body),
                  SM.remove arg (prune_tenv tenv body),
                  f,arg,body)
    | TApp (e',t) ->
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
    | TCase (tyop,alpha,
                eint,ebool,estr,
                efun,epair,elist) ->
      let closed_alpha = eval_typ tenv alpha in
      (match closed_alpha with
       | BoolT -> eval_loop env tenv ebool
       | IntT -> eval_loop env tenv eint
       | StrT -> eval_loop env tenv estr
       | FunT (t1,t2) ->
         eval_loop env tenv (TApp (TApp(efun,t1),t2))
       | PairT (t1,t2) ->
         eval_loop env tenv (TApp (TApp(epair,t1),t2))
       | ListT t' ->
         eval_loop env tenv (TApp (elist,t'))
       | _ -> raise (BadTCase e))
;;

(* evaluate closed, top-level expression e *)

let eval (e:exp) =
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
