open Common
module TS = TypedSyntax  
open ParsedSyntax
open Util.GenVar

exception Conversion_error of string ;;

let rec convert_kind (k:kind) : TS.kind =
  match k with
  | TypeK -> TS.TypeK
  | ArrowK (k1,k2) -> TS.ArrowK (convert_kind k1,
                                 convert_kind k2)
  | NoneK -> raise (Conversion_error "missing kind annotation")

let rec convert_typ (t:typ) : TS.typ =
  match t with
  | BoolT -> TS.BoolT
  | IntT -> TS.IntT
  | StrT -> TS.StrT
  | VoidT -> TS.VoidT
  | FunT (t1,t2) -> TS.FunT (convert_typ t1,
				 convert_typ t2)
  | PairT (t1,t2) -> TS.PairT (convert_typ t1,
				   convert_typ t2)
  | ListT t' -> TS.ListT (convert_typ t')
  | ForallT (v,k,t') -> TS.ForallT (v,convert_kind k,convert_typ t')
  | VarT x -> TS.VarT x
  | TFunT (v,k,t') -> TS.TFunT (v,convert_kind k,
                            convert_typ t')
  | TRecT (f,v,k1,k2,t') -> TS.TRecT (f,v,convert_kind k1,
                                      convert_kind k2, convert_typ t')
  | TAppT (t1,t2) -> TS.TAppT (convert_typ t1,
                               convert_typ t2)
  | TCaseT (alpha,t1,t2,t3,t4,t5,t6) ->
    TS.TCaseT (convert_typ alpha, convert_typ t1, convert_typ t2,
               convert_typ t3, convert_typ t4, convert_typ t5,
               convert_typ t6)
  | NoneT -> raise (Conversion_error "missing type annotation")


let rec _free_tvars_in_typ t bound =
  match t with
  | BoolT | IntT | StrT | VoidT | NoneT -> SS.empty
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

let multi_sub_in_typ (map:typ SM.t) (t:typ) : typ =
  SM.fold (fun v tv t -> sub_in_typ t v tv) map t

let rec convert (e:exp) : TS.exp =
  match e with
  | Var v -> TS.Var v   
  | Constant c -> TS.Constant c
  | Op (e1,op,e2) -> TS.Op (convert e1,op,convert e2)
  | If (e1,e2,e3) -> TS.If (convert e1,convert e2,convert e3)
  | Pair (e1,e2) -> TS.Pair (convert e1,convert e2)
  | Fst e' -> TS.Fst (convert e')
  | Snd e' -> TS.Snd (convert e')
  | EmptyList t -> TS.EmptyList (convert_typ t)
  | Cons (e1,e2) -> TS.Cons (convert e1, convert e2)
  | Match (e1,e2,v1,v2,e3) -> TS.Match (convert e1, convert e2,
					v1,v2, convert e3)
  | App (e1,e2) -> TS.App (convert e1,convert e2)
  | Fun (v,t,e') -> TS.Fun (v,convert_typ t, convert e')
  | Rec (v1,v2,t1,t2,e') -> TS.Rec(v1,v2,convert_typ t1,
				   convert_typ t2, convert e')
  | TFun (v,k,e') -> TS.TFun (v,convert_kind k,convert e')
  | TLet (v,t,e') -> raise (Conversion_error ("failed to eliminate type let"))
  | TApp (e',t) -> TS.TApp (convert e', convert_typ t)
  | TRec (v1,v2,k,t,e) -> TS.TRec (v1,v2,convert_kind k,convert_typ t,convert e)
  | TCase (annot,t,
              eint,ebool,estr,
              efun,epair,elist) ->
       TS.TCase (convert_typ annot,convert_typ t,
                    convert eint, convert ebool, convert estr,
                    convert efun,convert epair,convert elist)

let type_let =
  walk
    (fun _ e ->
       match e with
       | TLet (_,_,e') -> e'
       | _ -> e)
    multi_sub_in_typ
    (fun st e ->
       match e with
       | TLet (v,t,e') -> SM.add v t st
       | _ -> st)
    SM.empty

let transformations = [type_let] ;;

let transform_convert e =
  let e' = List.fold_left (fun e' f -> f e') e transformations in
  convert e'
