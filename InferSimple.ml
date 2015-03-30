open Common
open TypedSyntax ;;
  
exception Type_error of string ;;
  
type context = (variable * typ) list

(* Indicates that the two types should unify *)				
type type_constraint = typ * typ
				
let op_type (op:operator) : typ =
  match op with
  | Plus | Minus | Times | Div -> IntTyp
  | Less | LessEq -> BoolTyp

let expect (t1:typ) (t2:typ) : unit =
  if t1 <> t2 then
    raise (Type_error ("expected " ^ (string_of_typ t1) ^ ", got " ^ (string_of_typ t2)))

let gen_var_typ (id:int) : typ =
  VarTyp ("'a" ^ (string_of_int id))
	  
let rec accum_type_vars_in_typ (t:typ) : SS.t =
  match t with
  | BoolTyp | IntTyp -> SS.empty
  | FunTyp (t1,t2) | PairTyp (t1,t2) ->
		      SS.union (accum_type_vars_in_typ t1)
			       (accum_type_vars_in_typ t2)
  | ListTyp t1 -> accum_type_vars_in_typ t1
  | VarTyp v -> SS.singleton v					 
  			       
  
let accum_type_vars_in_exp (e:exp) : SS.t =
  let rec get_list (e:exp) : typ list =
    match e with
    | Var _ | Constant _ -> []
    | Fst e1 | Snd e1 -> get_list e1			      
    | Op (e1,_,e2) | Pair (e1,e2)
    | Cons (e1,e2) | App (e1,e2) -> (get_list e1) @ (get_list e2)
    | If (e1,e2,e3) -> (get_list e1) @ (get_list e2) @ (get_list e3)
    | Let (_,e1,e2) -> (get_list e1) @ (get_list e2)
    | Match (e1,e2,_,_,e3) -> (get_list e1) @ (get_list e2) @ (get_list e3)
    | EmptyList t -> [t]
    | Rec (_,_,t1,t2,e1) -> t1::t2::(get_list e1)
    | Closure _ -> raise (Type_error "there shouldn't be a closure here")
  in
  let all_types = get_list e in
  List.fold_left
         (fun set t ->
           SS.union set (accum_type_vars_in_typ t)
         )
         SS.empty all_types
  		   
let rec sub_in_typ (t:typ) (map:typ SM.t) : typ =
  match t with
  | BoolTyp | IntTyp -> t
  | FunTyp (t1,t2) -> FunTyp (sub_in_typ t1 map, sub_in_typ t2 map)
  | PairTyp (t1,t2) -> PairTyp (sub_in_typ t1 map, sub_in_typ t2 map)
  | ListTyp t1 -> ListTyp (sub_in_typ t1 map)
  | VarTyp x -> try SM.find x map with Not_found -> t

let rec sub_in_exp (e:exp) (map:typ SM.t) : exp =
  match e with
  | Var _ | Constant _ -> e
  | Op (e1,op,e2) -> Op (sub_in_exp e1 map, op, sub_in_exp e2 map)
  | If (e1,e2,e3) -> If (sub_in_exp e1 map,
			 sub_in_exp e2 map,
			 sub_in_exp e3 map)
  | Let (x,e1,e2) -> Let (x,sub_in_exp e1 map, sub_in_exp e2 map)
  | Pair (e1,e2) -> Pair (sub_in_exp e1 map, sub_in_exp e2 map)
  | Fst e1 -> Fst (sub_in_exp e1 map)
  | Snd e1 -> Snd (sub_in_exp e1 map)
  | EmptyList t -> EmptyList (sub_in_typ t map)
  | Cons (e1,e2) -> Cons (sub_in_exp e1 map, sub_in_exp e2 map)
  | Match (e1,e2,x,y,e3) -> Match (sub_in_exp e1 map,
				   sub_in_exp e2 map, x, y,
				   sub_in_exp e3 map)
  | Rec (x,y,t1,t2,e1) -> Rec (x,y,
			       sub_in_typ t1 map,
			       sub_in_typ t2 map,
			       sub_in_exp e1 map)
  | Closure _ -> raise (Type_error "assjew")
  | App (e1,e2) -> App (sub_in_exp e1 map, sub_in_exp e2 map)

let rename_typ_vars (e:exp) : exp * int =
  let vars = accum_type_vars_in_exp e in
  let (map,num_vars) =
    SS.fold
      (fun v (sm,n) -> (SM.add v (gen_var_typ n) sm, n+1))
      vars (SM.empty, 0)
  in
  (sub_in_exp e map, num_vars)
	  
	 
let rec typeof_ (ctx : context) (e : exp) (id:int) :
	  (typ * (type_constraint list) * int) =
  match e with
  | Var v ->
     (try (List.assoc v ctx, [], id)
      with Not_found -> raise (Type_error ("unbound variable " ^ v)))
  | Constant c ->
      (match c with
       | Int n -> (IntTyp, [], id)
       | Bool b -> (BoolTyp, [], id))
  | Op (e1,op,e2) ->
     let (e1_typ, cs1, id')  = typeof_ ctx e1 id in
     let (e2_typ, cs2, id'') = typeof_ ctx e2 id' in
     let cs = (e1_typ,IntTyp)::(e2_typ,IntTyp)::(cs1 @ cs2) in
     (op_type op, cs, id'')
  | If (cond,e1,e2) ->
     let (cond_typ, cs_cond, id') = typeof_ ctx cond id in
     let (e1_typ, cs1, id'') = typeof_ ctx e1 id' in
     let (e2_typ, cs2, id''') = typeof_ ctx e2 id'' in
     let cs = (cond_typ,BoolTyp)::(e1_typ,e2_typ)::(cs_cond @ cs1 @ cs2) in
     (e1_typ, cs, id''')
  | Let (v, e1, e2) ->
     let e1_typ, cs1, id' = typeof_ ctx e1 id in
     let new_ctx = (v,e1_typ) :: ctx in
     let e2_typ, cs2, id'' = typeof_ new_ctx e2 id' in
     (e2_typ, (cs1 @ cs2), id'')
  | Pair (e1,e2) ->
     let (e1_typ, cs1, id') = typeof_ ctx e1 id in
     let (e2_typ, cs2, id'') = typeof_ ctx e2 id' in
     (PairTyp (e1_typ, e2_typ), cs1 @ cs2, id'')
  | Fst p ->
     let p_typ, cs, id' = typeof_ ctx p id in
     let vartyp1 = gen_var_typ id' in
     let vartyp2 = gen_var_typ (id'+1) in
     let constr = (p_typ, PairTyp(vartyp1, vartyp2)) in
     (vartyp1, constr::cs, id'+2)
  | Snd p ->
     let p_typ, cs, id' = typeof_ ctx p id in
     let vartyp1 = gen_var_typ id' in
     let vartyp2 = gen_var_typ (id'+1) in
     let constr = (p_typ, PairTyp(vartyp1, vartyp2)) in
     (vartyp2, constr::cs, id'+2)
  | EmptyList t -> (ListTyp t, [], id)
  | Cons (hd,tl) ->
     let (hd_typ, cshd, id') = typeof_ ctx hd id in
     let (tl_typ, cstl, id'') = typeof_ ctx tl id' in
     let constr = (ListTyp hd_typ, tl_typ) in
     (ListTyp hd_typ, constr::(cshd @ cstl), id'')
  | Match (match_on, empty_case, hd_var, tl_var, cons_case) ->
     let match_on_typ, cs1, id' = typeof_ ctx match_on id in
     let vartyp = gen_var_typ id' in
     let empty_case_typ, cs2, id'' = typeof_ ctx empty_case (id'+1) in
     let new_ctx = (hd_var, vartyp)::(tl_var,ListTyp vartyp)::ctx in
     let cons_case_typ, cs3, id''' = typeof_ new_ctx cons_case id'' in
     let constr1 = (match_on_typ, ListTyp vartyp) in
     let constr2 = (empty_case_typ, cons_case_typ) in
     (cons_case_typ, constr1::constr2::(cs1 @ cs2 @ cs3), id''') 
  | Rec (name,arg,arg_typ,body_typ,body) ->
     let rec_typ = FunTyp(arg_typ,body_typ) in
     let new_ctx = (name,rec_typ)::(arg,arg_typ)::ctx in
     let body_typ', cs, id' = typeof_ new_ctx body id in
     (rec_typ, (body_typ,body_typ')::cs, id')
  | Closure _ -> raise (Type_error "Can't typecheck closures")
  | App (e1,e2) ->
     let e1_typ, cs1, id' = typeof_ ctx e1 id in
     let e2_typ, cs2, id'' = typeof_ ctx e2 id' in
     let vartyp = gen_var_typ id'' in
     let constr = (e1_typ, FunTyp(e2_typ, vartyp)) in
     (vartyp, constr::(cs1 @ cs2), id''+1)
       
let typeof (e:exp) =
  let (renamed,next_id) = rename_typ_vars e in
  let t,cs,_ = typeof_ [] renamed next_id in
  (t,cs)

let compose (map1:typ SM.t) (map2:typ SM.t) : typ SM.t =
  let map2' = SM.map (fun t -> sub_in_typ t map1) map2 in
  let map2'_list = SM.bindings map2' in
  let merge k t1_opt t2_opt =
      match t1_opt, t2_opt with
      | Some t1, None -> t1_opt
      | _ -> t2_opt
  in	         
  SM.merge merge map1 map2'
    
let rec unify (cs:type_constraint list) : typ SM.t =
  match cs with
  | [] -> SM.empty
  | ((t1,t2)::cs') ->
     if t1 = t2 then unify cs' else
     (match t1,t2 with
      | VarTyp v, t | t, VarTyp v ->
	 let t_vars = accum_type_vars_in_typ t in
	 if SS.mem v t_vars then raise (Type_error "unification failed")
         else
	   let map = SM.singleton v t in
	   let map_list = SM.bindings map in
	   let subbed_constraints =
	     List.map (fun (s1,s2) -> (sub_in_typ s1 map,
				       sub_in_typ s2 map))
		      cs'
           in
	   let unified = unify subbed_constraints in
	   let ret = compose unified map in
	   let ret_list = SM.bindings ret in
	   ret
			     
      | FunTyp (t1,t2), FunTyp (t1',t2') -> unify ((t1,t1')::(t2,t2')::cs')
      | PairTyp (t1,t2), PairTyp (t1',t2') -> unify ((t1,t1')::(t2,t2')::cs')
      | ListTyp t, ListTyp t' -> unify ((t,t')::cs')
      | _ -> raise (Type_error "unification failed"))
				       
let map =
  Rec ("map", "f", VarTyp "'a4", VarTyp "'a2",
    Rec ("mapf", "l", VarTyp "'c", VarTyp "'d",
      Match (Var "l",
                    EmptyList (VarTyp "'e"),
        "hd", "tl", Cons (Op (App (Var "f", Var "hd"), Plus, Constant (Int 1)),
                          App (Var "mapf", Var "tl")))))

let t,cs = typeof map ;;

let vars_set = accum_type_vars_in_exp map ;;


print_endline (string_of_typ t) ;;
List.iter (fun (t1,t2) -> print_endline ((string_of_typ t1) ^ " == " ^ (string_of_typ t2))) cs ;;


SM.iter (fun v t -> print_endline (v ^ ": " ^ (string_of_typ t))) (unify cs) ;;		  
      

