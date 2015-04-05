open TypedSyntax
open Common
open Type
  
(* Finds type of an expression given annotated function
arguments *)

exception Type_error of string ;;				
				
let op_type (op:operator) : typ =
  match op with
  | Plus | Minus | Times | Div | Mod ->
			    FunTyp (IntTyp, FunTyp (IntTyp, IntTyp))
  | Eq | Less | LessEq -> FunTyp (IntTyp, FunTyp (IntTyp, BoolTyp))
  | And | Or -> FunTyp (BoolTyp, FunTyp (BoolTyp, BoolTyp))
  | Concat -> FunTyp (StrTyp, (FunTyp (StrTyp, StrTyp)))

let expect (t1:typ) (t2:typ) : unit =
  if t1 <> t2 then
    raise (Type_error ("expected " ^ (Pretty.string_of_typ t1) ^
                       ", got " ^    (Pretty.string_of_typ t2)))

let rec is_polytype (t:typ) : bool =
  match t with
  | BoolTyp | IntTyp | StrTyp | VarTyp _ -> false
  | FunTyp (t1,t2) | PairTyp (t1,t2) -> is_polytype t1 || is_polytype t2
  | ListTyp u -> is_polytype u
  | Forall _ -> true

let rec typeof_ (ctx : typ SM.t) (tctx : unit SM.t) (e : exp) : typ =
  let bindings = SM.bindings ctx in
  match e with
  | Var v ->
    (try SM.find v ctx
     with Not_found -> raise (Type_error ("unbound variable " ^ v)))
  | Constant c ->
      (match c with
       | Int n -> IntTyp
       | Bool b -> BoolTyp
       | Str s -> StrTyp)
  | Op (e1,op,e2) ->
     let e1_typ = typeof_ ctx tctx e1 in
     let e2_typ = typeof_ ctx tctx e2 in
     (match op_type op with
      | FunTyp (t1,FunTyp(t2,t3)) ->
	 expect e1_typ t1 ; expect e2_typ t2 ;
	 t3
      | _ -> raise (Type_error "Expected a binary operator type"))
  | If (cond,e1,e2) ->
     let cond_typ = typeof_ ctx tctx cond in
     let e1_typ = typeof_ ctx tctx e1 in
     let e2_typ = typeof_ ctx tctx e2 in
     expect cond_typ BoolTyp ; expect e1_typ e2_typ ;
     e1_typ
  | Pair (e1,e2) ->
     let e1_typ = typeof_ ctx tctx e1 in
     let e2_typ = typeof_ ctx tctx e2 in
     PairTyp (e1_typ, e2_typ)
  | Fst p ->
     let p_typ = typeof_ ctx tctx p in
     (match p_typ with
      | PairTyp (t,_) -> t
      | _ -> raise (Type_error "Expected a Pair"))
  | Snd p ->
     let p_typ = typeof_ ctx tctx p in
     (match p_typ with
      | PairTyp (_,t) -> t
      | _ -> raise (Type_error "Expected a Pair"))
  | EmptyList t ->
     (match t with
      | ListTyp _ -> t
      | _ -> raise (Type_error "Expected a list"))
  | Cons (hd,tl) ->
     let hd_typ = typeof_ ctx tctx hd in
     let tl_typ = typeof_ ctx tctx tl in
     (match tl_typ with
      | ListTyp t -> expect hd_typ t ; tl_typ
      | _ -> raise (Type_error "Expected a List"))
  | Match (match_on, empty_case, hd_var, tl_var, cons_case) ->
     let match_on_typ = typeof_ ctx tctx match_on in
     (match match_on_typ with
      | ListTyp t ->
          let empty_typ = typeof_ ctx tctx empty_case in
          let new_ctx = SM.add hd_var t (SM.add tl_var (ListTyp t) ctx) in
	  let cons_typ = typeof_ new_ctx tctx cons_case in
	  expect empty_typ cons_typ ; empty_typ
      | _ -> raise (Type_error "Expected a List"))
  | Rec (name,arg,arg_typ,body_typ,body) ->
     let rec_typ = FunTyp(arg_typ,body_typ) in
     let new_ctx = SM.add name rec_typ  (SM.add arg arg_typ ctx) in
     let body_typ' = typeof_ new_ctx tctx body in
     expect body_typ body_typ' ; rec_typ
  | Fun (arg,arg_typ,body) ->
     let new_ctx = SM.add arg arg_typ ctx in
     FunTyp (arg_typ, typeof_ new_ctx tctx body)
  | App (e1,e2) ->
     (match typeof_ ctx tctx e1 with
      | FunTyp (t_in,t_out) ->
 	 let t2 = typeof_ ctx tctx e2 in
	 expect t2 t_in ; t_out
      | _ -> raise (Type_error "Expected a function"))
  | TypLam (v,e) ->
     let e_typ = typeof_ ctx (SM.add v () tctx) e in
     Forall (v, e_typ)
  | TypRec (f,v,t,body) ->
    let f_typ = Forall (v,t) in
    let body_typ = typeof_ (SM.add f f_typ ctx) (SM.add v () tctx) body in
    expect t body_typ; f_typ
  | TypApp (e,t) ->
    if is_polytype t
    then raise (Type_error "can't instantiate a type-function with a polytype")
    else
     let e_typ = typeof_ ctx tctx e in
     (match e_typ with
      | Forall (v, body_typ) -> Util.sub_in_typ body_typ v t
      | _ -> raise (Type_error "Expected a universal type"))

  | Typecase ((v,t),alpha,
              eint,ebool,estr,
              a,b,efun,
              c,d,epair,
              u,elist) ->

    let tbool = Util.sub_in_typ t v BoolTyp in
    let ebool_typ = typeof_ ctx tctx ebool in
    expect tbool ebool_typ ;

    let tint = Util.sub_in_typ t v IntTyp in
    let eint_typ = typeof_ ctx tctx eint in
    expect tint eint_typ ;

    let tfun = Util.sub_in_typ t v (FunTyp(VarTyp a,VarTyp b)) in
    let tctx' = SM.add b () (SM.add a () tctx) in
    let efun_typ = typeof_ ctx tctx' efun in
    expect tfun efun_typ ;

    let tpair = Util.sub_in_typ t v (PairTyp(VarTyp c,VarTyp d)) in
    let tctx' = SM.add d () (SM.add c () tctx) in
    let epair_typ = typeof_ ctx tctx' epair in
    expect tpair epair_typ ;

    let tlist = Util.sub_in_typ t v (ListTyp (VarTyp u)) in
    let tctx' = SM.add u () tctx in
    let elist_typ = typeof_ ctx tctx' elist in
    expect tlist elist_typ ;

    Util.sub_in_typ t v alpha
       
let typeof (e:exp) = typeof_ SM.empty SM.empty e
			     
