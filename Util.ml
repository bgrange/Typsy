
open SharedSyntax
open Type  
module SS = Set.Make(String) ;;
module SM = Map.Make(String) ;;
module ES = EvalSyntax

open TypedSyntax

let next_varid = ref 0 ;;  
let rec gen_var (avoid:SS.t) () : variable =
  let tvar = "'a" ^ (string_of_int !next_varid) in
  next_varid := !next_varid + 1 ;
  if not (SS.mem tvar avoid) then tvar else gen_var avoid ()
;;

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
    | TypLam (_,e') -> aux e' bound
    | TypApp (e',_) -> aux e' bound			   
  in aux e SS.empty
;;

let rec _free_tvars_in_typ t bound =
  match t with
  | BoolTyp | IntTyp -> SS.empty
  | FunTyp (t1,t2) | PairTyp (t1,t2) ->
		      SS.union (_free_tvars_in_typ t1 bound)
			       (_free_tvars_in_typ t2 bound)
  | ListTyp t1 -> _free_tvars_in_typ t1 bound
  | Forall (v,t') -> _free_tvars_in_typ t' (SS.add v bound)		       
  | VarTyp v -> if SS.mem v bound then SS.empty else SS.singleton v
  
  
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
    | TypApp (e',t) -> SS.union (aux e' bound) (_free_tvars_in_typ t bound)
    | TypLam (v,e) -> aux e (SS.add v bound)
  in
  aux e SS.empty



(* u can have free type variables in it, hence the
renaming *)  
let rec sub_in_typ (t:typ) (v:variable) (u:typ) : typ =
  let fvars = free_tvars_in_typ u in
  let rec aux t =
    match t with
    | BoolTyp | IntTyp -> t
    | FunTyp (t1,t2) -> FunTyp (aux t1, aux t2)
    | PairTyp (t1,t2) -> PairTyp (aux t1, aux t2)
    | ListTyp t1 -> ListTyp (aux t1)
    | VarTyp v' -> if var_eq v v' then u else t
    | Forall (v',t') -> if var_eq v v' then t
			else if SS.mem v' fvars then
			  let avoid_vars = SS.union fvars (free_tvars_in_typ t') in
			  let w = gen_var avoid_vars () in
			  let t'_renamed = sub_in_typ t' v' (VarTyp w) in
			  Forall (w, aux t'_renamed)
		        else
			  Forall (v',aux t')
  in
  aux t


let rec erase_types (te:exp) : ES.exp =
  match te with
  | Var v -> ES.Var v
  | Constant c -> ES.Constant c
  | Op (e1,op,e2) -> ES.Op ((erase_types e1), op,
                               (erase_types e2))
  | If (e1,e2,e3) -> ES.If ((erase_types e1),
                               (erase_types e2),
                               (erase_types e3))
  | Pair (e1,e2) -> ES.Pair ((erase_types e1), (erase_types e2))
  | Fst e -> ES.Fst (erase_types e)
  | Snd e -> ES.Snd (erase_types e)
  | EmptyList _ -> ES.EmptyList
  | Cons (e1,e2) -> ES.Cons ((erase_types e1), (erase_types e2))                         
  | Match (e1,e2,v1,v2,e3) -> ES.Match ((erase_types e1), (erase_types e2),
                                           v1,v2, (erase_types e3))
  | App (e1,e2) -> ES.App (erase_types e1,erase_types e2)
  | Fun (v,_,e) -> ES.Fun (v,erase_types e,
                              free_vars e,
                              free_tvars e)
  | Rec (v1,v2,_,_,e) -> ES.Rec (v1,v2,erase_types e,
                                    free_vars e,
                                    free_tvars e)
  | TypLam (v,e) -> ES.TypLam (v, erase_types e,
                               free_vars e,
                               free_tvars e)
  | TypApp (e,t) -> ES.TypApp (erase_types e, t)
