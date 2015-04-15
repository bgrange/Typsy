open Common
open TypedSyntax

module GenVar :
sig
  val gen_var : SS.t -> unit -> variable
end =
struct
let next_varid = ref 0 ;;  
let rec gen_var (avoid:SS.t) () : variable =
  let tvar = "'a" ^ (string_of_int !next_varid) in
  next_varid := !next_varid + 1 ;
  if not (SS.mem tvar avoid) then tvar else gen_var avoid ()
end

open GenVar

let rec _free_tvars_in_typ t bound =
  match t with
  | BoolT | IntT | StrT | VoidT -> SS.empty
  | FunT (t1,t2) | PairT (t1,t2)
  | TAppT (t1,t2) ->
    SS.union (_free_tvars_in_typ t1 bound)
      (_free_tvars_in_typ t2 bound)
  | ListT t1 -> _free_tvars_in_typ t1 bound
  | ForallT (v,k,t') | TFunT (v,k,t') -> _free_tvars_in_typ t' (SS.add v bound)
  | TRecT (f,v,k1,k2,t') -> _free_tvars_in_typ t' (SS.add f (SS.add v bound))
  | VarT v -> if SS.mem v bound then SS.empty else SS.singleton v
  | TCaseT (t1,t2,t3,t4,t5,t6,t7) ->
    let types = [t1;t2;t3;t4;t5;t6;t7] in
    List.fold_right
      (fun t acc -> SS.union (_free_tvars_in_typ t bound) acc)
      types SS.empty
    
let free_tvars_in_typ (t:typ) : SS.t =
  _free_tvars_in_typ t SS.empty
;;

let free_tvars (e:exp) : SS.t =
  let rec aux e bound =
    match e with
    | Var _ | Constant _ -> SS.empty
    | Op (e1,_,e2) | Pair(e1,e2) | Cons (e1,e2) |
      App (e1,e2) ->
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | If (e1,e2,e3) | Match (e1,e2,_,_,e3) ->
        SS.union (SS.union (aux e1 bound)
                           (aux e2 bound))
                           (aux e3 bound)
    | Fst e' | Snd e' -> aux e' bound
    | Rec (_,_,t1,t2,e') ->
       SS.union (SS. union (_free_tvars_in_typ t1 bound)
			   (_free_tvars_in_typ t2 bound))
		(aux e' bound)
    | Fun (_,t,e') ->
       SS.union (_free_tvars_in_typ t bound) (aux e' bound)
    | EmptyList t -> _free_tvars_in_typ t bound
    | TApp (e',t) -> SS.union (aux e' bound) (_free_tvars_in_typ t bound)
    | TFun (v,_,e) -> aux e (SS.add v bound)
    | TRec (f,v,_,t,e') ->
      SS.union
        (_free_tvars_in_typ t bound)
        (aux e' (SS.add v bound))
    | Closure _ | RecClosure _ -> SS.empty
    | TCase (tyop,alpha,
                eint,ebool,estr,
                efun,epair,elist) ->
      let exps = [eint;ebool;efun;epair;elist] in
      List.fold_right (fun e acc -> SS.union (aux e bound) acc)
        exps (_free_tvars_in_typ alpha bound)
  in     
  aux e SS.empty

(* u can have free type variables in it, hence the
renaming *)  
let rec sub_in_typ (t:typ) (v:variable) (u:typ) : typ =
  let fvars = free_tvars_in_typ u in
  let rec aux t =
    match t with
    | BoolT | IntT | StrT | VoidT -> t
    | FunT (t1,t2) -> FunT (aux t1, aux t2)
    | PairT (t1,t2) -> PairT (aux t1, aux t2)
    | ListT t1 -> ListT (aux t1)
    | VarT v' -> if var_eq v v' then u else t
    | ForallT (v',k,t') ->
      if var_eq v v' then t
      else if SS.mem v' fvars then
	let avoid_vars = SS.union fvars (free_tvars_in_typ t') in
	let w = gen_var avoid_vars () in
	let t'_renamed = sub_in_typ t' v' (VarT w) in
        ForallT (w, k,aux t'_renamed)
      else
	ForallT (v',k,aux t')
    | TAppT (t1,t2) -> TAppT (aux t1, aux t2)
    | TFunT (v',k,t') ->
      if var_eq v v' then t
      else if SS.mem v' fvars then
	let avoid_vars = SS.union fvars (free_tvars_in_typ t') in
	let w = gen_var avoid_vars () in
	let t'_renamed = sub_in_typ t' v' (VarT w) in
	TFunT (w, k,aux t'_renamed)
      else
	TFunT (v',k,aux t')
    | TRecT (f,w,k1,k2,t') ->
      if var_eq v f || var_eq v w then t
      else if SS.mem w fvars || SS.mem f fvars then
        let avoid_vars = SS.union fvars (free_tvars_in_typ t') in
        let f' = gen_var avoid_vars () in
        let w' = gen_var avoid_vars () in
        let t'_renamed = sub_in_typ (sub_in_typ t' w (VarT w')) f (VarT f') in
        TRecT (f',w',k1,k2,aux t'_renamed)
      else
        TRecT (f,w,k1,k2,aux t')
    | TCaseT (t1,t2,t3,t4,t5,t6,t7) ->
      TCaseT (aux t1, aux t2,
              aux t3, aux t4,
              aux t5, aux t6, aux t7)
  in
  aux t

let multi_sub_in_typ (map:tenv) (t:typ) : typ =
  SM.fold (fun v tv t -> sub_in_typ t v tv) map t

let free_vars (e:exp) : SS.t =
  let rec aux e bound =
    match e with
      Var x ->
        if SS.mem x bound then SS.empty else SS.singleton x
    | Constant _ -> SS.empty
    | Op (e1,op,e2) ->
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | If (e1,e2,e3) ->
        SS.union (SS.union (aux e1 bound)
                           (aux e2 bound))
                           (aux e3 bound)
    | Pair (e1,e2) ->
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | Fst p -> aux p bound
    | Snd p -> aux p bound
    | EmptyList _ -> SS.empty 
    | Cons (hd,tl) -> 
        SS.union (aux hd bound)
                 (aux tl bound)
    | Match (e1,e2,x_hd,x_tl,e3) ->
        SS.union (SS.union (aux e1 bound)
                           (aux e2 bound))
                 (aux e3 (SS.add x_hd
				 (SS.add x_tl bound)))
    | Rec (name,arg,_,_,body) ->
       aux body (SS.add name (SS.add arg bound))
    | Fun (arg,_,body) ->
       aux body (SS.add arg bound)
    | App (e1,e2) ->  
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | TFun (_,_,e') -> aux e' bound
    | TApp (e',_) -> aux e' bound
    | TRec (f,v,k,t,e') -> aux e' bound
    | Closure _ | RecClosure _ -> SS.empty
    | TCase (tyop,alpha,
                eint,ebool,estr,
                efun,epair,elist) ->
      let exps = [eint;ebool;efun;epair;elist] in
      List.fold_right (fun e acc -> SS.union (aux e bound) acc) exps SS.empty
        
  in aux e SS.empty
;;

let rec normalize_type (t:typ) : typ =
  match t with
  | BoolT | IntT | StrT | VarT _ | VoidT -> t
  | FunT (t1,t2) -> FunT (normalize_type t1,
                          normalize_type t2)
  | PairT (t1,t2) -> PairT (normalize_type t1,
                            normalize_type t2)
  | ListT u -> ListT (normalize_type u)
  | ForallT (v,k,t') -> ForallT (v,k,normalize_type t')
  | TFunT (v,k,t') -> TFunT (v,k,normalize_type t')
  | TRecT (f,v,k1,k2,t') -> TRecT (f,v,k1,k2,normalize_type t')
  | TAppT (t1,t2) ->
    let t1' = normalize_type t1 in
    let t2' = normalize_type t2 in
    (match t1' with
     | TFunT (v,k,t1'_body) ->
       let subbed = sub_in_typ t1'_body v t2' in
       normalize_type subbed
     | TRecT (f,v,k1,k2,t1'_body) ->
       let subbed = sub_in_typ t1'_body v t2' in
       let subbed = sub_in_typ subbed f t1' in
       normalize_type subbed
     | _ -> TAppT (t1',t2'))
  | TCaseT (alpha,tint,tbool,tstr,
            tfun,tpair,tlist) ->
    let alpha = normalize_type alpha in
    (* typechecker guarantees that alpha has kind *
       and isn't a universal type *)
    (match alpha with
     | IntT -> tint
     | BoolT -> tbool
     | StrT -> tstr
     | FunT (a,b) -> normalize_type (TAppT (TAppT (tfun, a),b))
     | PairT (a,b) -> normalize_type (TAppT (TAppT (tpair, a), b))
     | ListT a -> normalize_type (TAppT (tlist,a))
     | VarT _ -> t
     | _ -> raise (Failure "bad Typecase"))

(*

(* u should be a closed type, t can be open *)  
(* raise exception if unification fails *)
let rec unify (t:typ)  (u:typ) : typ SM.t =
  let merge_fun k b1_opt b2_opt =
    match b1_opt, b2_opt with
    | None, Some b | Some b, None -> Some b
    | Some b1, Some b2 -> ignore (unify b1 b2) ; Some b1
                    (*
                    if typ_eq b1 b2 then Some b1
                    else raise (Failure "unification failed")*)
    | None, None -> None
  in
  match t, u with
  | IntT,IntT | BoolT,BoolT -> SM.empty
  | VarT v, _ -> SM.singleton v u			     
  | FunT (t1,t2), FunT (u1,u2)
  | PairT (t1,t2), PairT (u1,u2) -> SM.merge merge_fun
                                        (unify t1 u1) (unify t2 u2)
  | ListT t', ListT u' -> unify t' u'
  | ForallT (tv, t'), ForallT (uv,u') ->
                  let u'' = sub_in_typ u' uv (VarT tv) in
                  SM.remove tv (unify t' u'')
  | _ -> raise (Failure "unification failed")
				       
         *)
