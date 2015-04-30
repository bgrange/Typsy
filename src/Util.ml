open Common
open Syntax

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

let transformt
    (f:'st -> typ -> typ)
    (update:'st -> typ -> 'st)
    (s:'st)
    (t:typ) : typ =
  let rec aux s t =
    let s' = update s t in
    let t' =
      match t with
      | BoolT | IntT | StrT | VoidT | VarT _ | NoneT -> t
      | FunT (a,b) -> FunT (aux s' a, aux s' b)
      | PairT (a,b) -> PairT (aux s' a, aux s' b)
      | ListT a -> ListT (aux s' a)
      | ForallT (v,k,a) -> ForallT (v,k,aux s' a)
      | TFunT (v,k,a) -> TFunT (v,k,aux s' a)
      | TAppT (a,b) -> TAppT (aux s' a, aux s' b)
      | TRecT (alpha,
                t1,t2,t3,t4,t5,t6,t7) ->
        TRecT (aux s' alpha,
                aux s' t1, aux s' t2, aux s' t3,
                aux s' t4, aux s' t5, aux s' t6, aux s' t7)

    in
    f s t'
  in
  aux s t

let foldt
    (join:'st -> typ -> 'x list -> 'x)
    (inject_k: 'st -> kind -> 'x)
    (inject:'st -> typ -> 'x)
    (update:'st -> typ -> 'st)
    (s:'st)
    (t:typ) : 'x =
  let rec aux s t =
    match t with
    | BoolT | IntT | StrT | VoidT | VarT _ | NoneT -> inject s t
    | _ ->
      let s' = update s t in
      let xs = 
        (match t with
         | FunT (a,b) -> [aux s' a; aux s' b]
         | PairT (a,b) -> [aux s' a; aux s' b]
         | ListT a -> [aux s' a]
         | ForallT (_,k,a) -> [inject_k s k;
                               aux s' a]
         | TFunT (_,k,a) -> [inject_k s k;
                             aux s' a]
         | TAppT (a,b) -> [aux s' a; aux s' b]
         | TRecT (alpha,
                   t1,t2,t3,t4,t5,t6,t7) ->
           [aux s' alpha;
            aux s' t1; aux s' t2; aux s' t3;
            aux s' t4; aux s' t5; aux s' t6; aux s' t7]
         | _ -> raise Impossible_error)
      in
      join s t xs
  in
  aux s t

let transform
    (f:'st -> exp -> exp)
    (g:'st -> typ -> typ)
    (update:'st -> exp -> 'st)
    (s:'st)
    (e:exp) : exp =
  let rec aux s e =
    let s' = update s e in
    let e' =
      match e with
      | Var _ | Constant _ -> e
      | Binop (e1,op,e2) -> Binop (aux s' e1,op,aux s' e2)
      | Unop (op,e) -> Unop (op, aux s' e)
      | If (e1,e2,e3) -> If (aux s' e1,aux s' e2, aux s' e3)
      | Pair (e1,e2) -> Pair (aux s' e1, aux s' e2)
      | Fst e1 -> Fst (aux s' e1)
      | Snd e1 -> Snd (aux s' e1)
      | EmptyList t -> EmptyList (g s t)
      | Cons (e1,e2) -> Cons (aux s' e1,aux s' e2)
      | Match (e1,e2,v1,v2,e3) -> Match (aux s' e1,aux s' e2,
                                         v1,v2,aux s' e3)
      | TCase (t,alpha,
               t1,t2,t3,t4,t5,t6,t7) ->
        TCase (g s t, g s alpha,
               aux s' t1, aux s' t2, aux s' t3,
               aux s' t4, aux s' t5, aux s' t6, aux s' t7)
      | App (e1,e2) -> App (aux s' e1, aux s' e2)
      | Fun (v,t,e1) -> Fun (v,g s t,
                             aux s' e1)
      | Rec (f,v,t1,t2,e1) -> Rec (f,v,
                                   g s t1,
                                   g s t2,
                                   aux s' e1)
      | TFun (v,k,e1) -> TFun (v,k,aux s' e1)
      | TRec (f,v,k,t,e1) -> TRec (f,v,k,g s t,aux s' e1)                           
      | TApp (e1,t) -> TApp (aux s' e1, g s t)
      | Closure _ | RecClosure _ -> raise Closure_error
    in
    f s e'
  in
  aux s e

let fold
    (join:'st -> exp -> 'x list -> 'x)
    (inject_k:'st -> kind -> 'x)
    (inject_t:'st -> typ -> 'x)
    (inject:'st -> exp -> 'x)
    (update:'st -> exp -> 'st)
    (s:'st)
    (e:exp) : 'x =

  let rec aux s e =
    match e with
    | Var _ | Constant _ -> inject s e
    | _ ->
      let s' = update s e in
      let xs = 
        (match e with
         | Binop (e1,op,e2) -> [aux s' e1;aux s' e2]
         | Unop (op,e) -> [aux s' e]
         | If (e1,e2,e3) -> [aux s' e1;aux s' e2;aux s' e3]
         | Pair (e1,e2) -> [aux s' e1;aux s' e2]
         | Fst e1 -> [aux s' e1]
         | Snd e1 -> [aux s' e1]
         | EmptyList t -> [inject_t s t]
         | Cons (e1,e2) -> [aux s' e1;aux s' e2]
         | Match (e1,e2,v1,v2,e3) -> [aux s' e1;aux s' e2;aux s' e3]
         | TCase (t,alpha,
                  e1,e2,e3,e4,e5,e6,e7) ->
           [inject_t s t; inject_t s alpha;
            aux s' e1; aux s' e2; aux s' e3;
            aux s' e4; aux s' e5; aux s' e6; aux s' e7]
         | App (e1,e2) -> [aux s' e1; aux s' e2]
         | Fun (v,t,e1) -> [inject_t s t;aux s' e1]
         | Rec (f,v,t1,t2,e1) -> [inject_t s t1;
                                  inject_t s t2;
                                  aux s' e1]
         | TFun (v,k,e1) -> [inject_k s k;aux s' e1]
         | TRec (f,v,k,t,e1) -> [inject_k s k;inject_t s t;aux s' e1]                            
         | TApp (e1,t) -> [aux s' e1; inject_t s t]
         | Closure _ | RecClosure _ -> raise Closure_error
         | _ -> raise Impossible_error)

      in
      join s e xs
  in
  aux s e

let _free_tvars_in_typ bound t : SS.t =
  let union_all sets = List.fold_right SS.union sets SS.empty in
  foldt
    (fun _ _ sets -> union_all sets)
    (fun _ _ -> SS.empty)
    (fun b t ->
       match t with
       | VarT v -> if SS.mem v b then SS.empty else SS.singleton v
       | _ -> SS.empty)
    (fun b t ->
       match t with
       | ForallT (v,_,_) | TFunT (v,_,_) -> SS.add v b
       | _ -> b)
    bound
    t  

let free_tvars_in_typ (t:typ) : SS.t =
  _free_tvars_in_typ SS.empty t
;;

let free_tvars (e:exp) : SS.t =
  let union_all sets = List.fold_right SS.union sets SS.empty in
  fold
    (fun _ _ sets -> union_all sets)
    (fun _ _ -> SS.empty)
    _free_tvars_in_typ
    (fun _ _ -> SS.empty)
    (fun s e ->
       match e with
       | TFun (v,_,_) -> SS.add v s
       | TRec (f,v,_,_,_) -> SS.add f (SS.add v s)                           
       | _ -> s)
    SS.empty
    e

(* u can have free type variables in it, hence the
renaming *)  
let rec sub_in_typ (t:typ) (v:variable) (u:typ) : typ =
  let fvars = free_tvars_in_typ u in
  let rec aux t =
    match t with
    | BoolT | IntT | StrT | VoidT | NoneT -> t
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
    | TRecT (t1,t2,t3,t4,t5,t6,t7,t8) ->
      TRecT (aux t1, aux t2,
              aux t3, aux t4,
              aux t5, aux t6, aux t7,
              aux t8)
  in
  aux t

(*
let multi_sub_in_typ (map:tenv) (t:typ) : typ =
  SM.fold (fun v tv t -> sub_in_typ t v tv) map t*)

let sub_typ_in_exp (e:exp) (v:variable) (t:typ) : exp =
  transform
    (fun _ e -> e)
    (fun b u -> if SS.mem v b then t else sub_in_typ u v t)
    (fun s e ->
       match e with
       | TFun (v,_,_) -> SS.add v s
       | TRec (f,v,_,_,_) -> SS.add f (SS.add v s)                           
       | _ -> s)
    SS.empty
    e
  

let free_vars (e:exp) : SS.t =
  let rec aux e bound =
    match e with
      Var x ->
        if SS.mem x bound then SS.empty else SS.singleton x
    | Constant _ -> SS.empty
    | Binop (e1,op,e2) ->
        SS.union (aux e1 bound)
          (aux e2 bound)
    | Unop (op,e) -> aux e bound
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
    | TRec (f,v,k,t,e') -> aux e' bound
    | TApp (e',_) -> aux e' bound
    | Closure _ | RecClosure _ -> SS.empty
    | TCase (tyop,alpha,
                eint,ebool,estr,evoid,
                efun,epair,elist) ->
      let exps = [eint;ebool;estr;evoid;efun;epair;elist] in
      List.fold_right (fun e acc -> SS.union (aux e bound) acc) exps SS.empty
        
  in aux e SS.empty
;;

exception BadTypecase

let rec normalize_type (t:typ) : typ =
  match t with
  | BoolT | IntT | StrT | VarT _ | VoidT | NoneT -> t
  | FunT (t1,t2) -> FunT (normalize_type t1,
                          normalize_type t2)
  | PairT (t1,t2) -> PairT (normalize_type t1,
                            normalize_type t2)
  | ListT u -> ListT (normalize_type u)
  | ForallT (v,k,t') -> ForallT (v,k,normalize_type t')
  | TFunT (v,k,t') -> TFunT (v,k,normalize_type t')
  | TAppT (t1,t2) ->
    let t1' = normalize_type t1 in
    let t2' = normalize_type t2 in
    (match t1' with
     | TFunT (v,k,t1'_body) ->
       let subbed = sub_in_typ t1'_body v t2' in
       normalize_type subbed
     | _ -> TAppT (t1',t2'))
  | TRecT (alpha,tint,tbool,tstr,tvoid,
            tfun,tpair,tlist) ->
    let alpha = normalize_type alpha in
    (* typechecker guarantees that alpha has kind *
       and isn't a universal type *)
    let tint = normalize_type tint in
    let tbool = normalize_type tbool in
    let tstr = normalize_type tstr in
    let tvoid = normalize_type tvoid in
    let tfun = normalize_type tfun in
    let tpair = normalize_type tpair in
    let tlist = normalize_type tlist in

    let typecase_of s = TRecT (s,tint,tbool,tstr,tvoid,tfun,tpair,tlist) in
    (match alpha with
     | IntT -> tint
     | BoolT -> tbool
     | StrT -> tstr
     | VoidT -> tvoid
     | FunT (a,b) -> normalize_type (TAppT (TAppT (TAppT (TAppT (tfun, a),b),
                                              typecase_of a), typecase_of b))
     | PairT (a,b) -> normalize_type (TAppT (TAppT (TAppT (TAppT (tpair, a),b),
                                              typecase_of a), typecase_of b))


     | ListT a -> normalize_type (TAppT (TAppT (tlist,a), typecase_of a))
     | VarT _ | TAppT _ | TRecT _ -> typecase_of alpha 
     | ForallT _ | TFunT _ -> raise BadTypecase
     | NoneT -> raise Missing_type)


let rec typ_eq t u =
  match t, u with
  | BoolT, BoolT
  | IntT, IntT
  | StrT, StrT
  | VoidT, VoidT -> true
  | FunT (t1,t2), FunT (u1,u2)
  | PairT (t1,t2), PairT (u1,u2)
  | TAppT (t1,t2) , TAppT (u1,u2) -> (typ_eq t1 u1) && (typ_eq t2 u2)
  | ListT t', ListT u' -> typ_eq t' u'
  | ForallT (tv,tk,t'), ForallT (uv,uk,u')
  | TFunT (tv,tk,t'), TFunT (uv,uk,u') ->
    let u' = sub_in_typ u' uv (VarT tv) in
    typ_eq t' u'
  | TRecT (alpha,t1,t2,t3,t4,t5,t6,t7), TRecT (beta,u1,u2,u3,u4,u5,u6,u7) ->
    List.for_all2 typ_eq
      [alpha;t1;t2;t3;t4;t5;t6;t7]
      [beta;u1;u2;u3;u4;u5;u6;u7]
  | VarT v, VarT w -> var_eq v w
  | _ -> false

let typ_equiv (t:typ) (u:typ) : bool =
  typ_eq (normalize_type t) (normalize_type u)

(* Generalization of substitution where the thing being
   replaced can be any type expression. Types are compared
   with typ_eq (not typ_equiv) *)
let rec replace_typ (t:typ) (v:typ) (u:typ) : typ =
  let ufvars = free_tvars_in_typ u in
  let vfvars = free_tvars_in_typ v in
  let rec aux t =
    if typ_eq t v then u else
    match t with
    | BoolT | IntT | StrT | VoidT | NoneT | VarT _ -> t
    | FunT (t1,t2) -> FunT (aux t1, aux t2)
    | PairT (t1,t2) -> PairT (aux t1, aux t2)
    | ListT t1 -> ListT (aux t1)
    | ForallT (v',k,t') ->
      if SS.mem v' vfvars then t
      else if SS.mem v' ufvars then
	let avoid_vars = SS.union ufvars (free_tvars_in_typ t') in
	let w = gen_var avoid_vars () in
	let t'_renamed = sub_in_typ t' v' (VarT w) in
        ForallT (w, k,aux t'_renamed)
      else
	ForallT (v',k,aux t')
    | TAppT (t1,t2) -> TAppT (aux t1, aux t2)
    | TFunT (v',k,t') ->
      if SS.mem v' vfvars then t
      else if SS.mem v' ufvars then
	let avoid_vars = SS.union ufvars (free_tvars_in_typ t') in
	let w = gen_var avoid_vars () in
	let t'_renamed = sub_in_typ t' v' (VarT w) in
	TFunT (w, k,aux t'_renamed)
      else
	TFunT (v',k,aux t')
    | TRecT (t1,t2,t3,t4,t5,t6,t7,t8) ->
      TRecT (aux t1, aux t2,
              aux t3, aux t4,
              aux t5, aux t6, aux t7,
              aux t8)
  in
  aux t

