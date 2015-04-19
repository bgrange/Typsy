open TypedSyntax
open Common
  
(* Finds type of an expression given annotated function
arguments *)

exception Type_error of string ;;

let unbound_var (v:string) =
  raise (Type_error ("unbound variable " ^ v))

let mismatchk k1 k2 =
  raise (Type_error ("expected kind " ^ (Pretty.string_of_kind k1) ^
                     ", got kind " ^ (Pretty.string_of_kind k2)))

let mismatchk' s k =
  raise (Type_error ("expected kind " ^ s ^
                     ", got kind " ^ (Pretty.string_of_kind k)))

let expectk (k1:kind) (k2:kind) : unit =
  if k1 <> k2 then mismatchk k1 k2

let rec kindof_ (ctx : kind SM.t) (t:typ) =
  match t with
  | BoolT | IntT | StrT | VoidT -> TypeK
  | FunT (t1,t2) | PairT (t1,t2) ->
    let k1 = kindof_ ctx t1 in
    let k2 = kindof_ ctx t2 in
    expectk TypeK k1;
    expectk TypeK k2;
    TypeK
  | ListT u ->
    let k = kindof_ ctx u in
    expectk TypeK k;
    TypeK
  | ForallT (v,k,t') ->
    let kall = kindof_ (SM.add v k ctx) t' in
    expectk TypeK kall ;
    kall
  | VarT v ->
    (try SM.find v ctx
     with Not_found -> unbound_var v)
  | TFunT (v,k,u) ->
    let body_kind = kindof_ (SM.add v k ctx) u in
    ArrowK (k,body_kind)
  | TRecT (f,v,k1,k2,u) ->
    let kf = ArrowK (k1,k2) in
    let ku = kindof_ (SM.add f kf (SM.add v k1 ctx)) u in
    expectk ku k2 ;
    kf
  | TAppT (t1,t2) ->
    let k1 = kindof_ ctx t1 in
    let k2 = kindof_ ctx t2 in
    (match k1 with
     | TypeK -> mismatchk' "arrow" k1
     | ArrowK (k11,k12) ->
       expectk k11 k2;
       k12)
  | TCaseT (alpha,tint,tbool,tstr,tvoid,
            tfun,tpair,tlist) ->
    expectk TypeK (kindof_ ctx alpha) ;
    let k = kindof_ ctx tint in
    expectk k (kindof_ ctx tbool) ;
    expectk k (kindof_ ctx tstr) ;
    expectk k (kindof_ ctx tvoid) ;

    let star_k = ArrowK (TypeK,k) in
    let star_star_k = ArrowK (TypeK, star_k) in
    expectk star_star_k (kindof_ ctx tfun) ;
    expectk star_star_k (kindof_ ctx tpair) ;
    expectk star_k (kindof_ ctx tlist) ;
    k
    
let kindof t = kindof_ SM.empty t

let rec typ_eq t u =
  match t, u with
  | BoolT, BoolT
  | IntT, IntT
  | StrT, StrT
  | VoidT, VoidT -> true
  | FunT (t1,t2), FunT (u1,u2)
  | PairT (t1,t2), PairT (u1,u2) -> (typ_eq t1 u1) && (typ_eq t2 u2)
  | ListT t', ListT u' -> typ_eq t' u'
  | ForallT (tv,tk,t'), ForallT (uv,uk,u')
  | TFunT (tv,tk,t'), TFunT (uv,uk,u') ->
    let u' = Util.sub_in_typ u' uv (VarT tv) in
    typ_eq t' u'
  | TRecT (tf,tv,tk1,tk2,t'), TRecT (uf,uv,uk1,uk2,u') ->
    let u' = Util.sub_in_typ u' uf (VarT tf) in
    let u' = Util.sub_in_typ u' uv (VarT tv) in
    typ_eq t' u'
  | TCaseT (alpha,t1,t2,t3,t4,t5,t6,t7), TCaseT (beta,u1,u2,u3,u4,u5,u6,u7) ->
    List.for_all2 typ_eq
      [alpha;t1;t2;t3;t4;t5;t6;t7]
      [beta;u1;u2;u3;u4;u5;u6;u7]
  | VarT v, VarT w -> var_eq v w
  | _ -> false

let typ_equiv (t:typ) (u:typ) : bool =
  let kt = kindof t in
  let ku = kindof u in
  if kt <> ku then false
  else typ_eq (Util.normalize_type t) (Util.normalize_type u)
				
let op_type (op:operator) : typ =
  match op with
  | Plus | Minus | Times | Div | Mod ->
			    FunT (IntT, FunT (IntT, IntT))
  | Eq | Less | LessEq -> FunT (IntT, FunT (IntT, BoolT))
  | And | Or -> FunT (BoolT, FunT (BoolT, BoolT))
  | Concat -> FunT (StrT, (FunT (StrT, StrT)))


let rec is_polytype (t:typ) : bool =
  match t with
  | BoolT | IntT | StrT | VarT _ | VoidT -> false
  | FunT (t1,t2) | PairT (t1,t2) | TAppT(t1,t2) ->
    is_polytype t1 || is_polytype t2
  | ListT u | TFunT (_,_,u) | TRecT (_,_,_,_,u) -> is_polytype u
  | TCaseT (alpha,t1,t2,t3,t4,t5,t6,t7) ->
    List.exists is_polytype [t1;t2;t3;t4;t5;t6;t7]
  | ForallT _ -> true

let check_polytype (t:typ) : unit =
  if is_polytype t then raise (Type_error "unexpected polytype")

let mismatch t1 t2 =
  raise (Type_error ("expected type " ^ (Pretty.string_of_typ t1) ^
                     ", got type " ^ (Pretty.string_of_typ t2)))

let mismatch' s t =
  raise (Type_error ("expected type " ^ s ^
                     ", got type " ^ (Pretty.string_of_typ t)))
    
let expect (t1:typ) (t2:typ) : unit =
  if not (typ_eq t1 t2) then mismatch t1 t2

(* I want to maintain the invariant that typeof_ *always* returns a
   normalized type. Usually this is easy *)
let rec typeof_ (ctx : typ SM.t) (tctx : kind SM.t) (e : exp) : typ =
  match e with
  | Var v ->
    (try SM.find v ctx
     with Not_found -> unbound_var v)
  | Constant c ->
      (match c with
       | Int n -> IntT
       | Bool b -> BoolT
       | Str s -> StrT)
  | Op (e1,op,e2) ->
     let e1_typ = typeof_ ctx tctx e1 in
     let e2_typ = typeof_ ctx tctx e2 in
     let opt = op_type op in
     (match opt with
      | FunT (t1,FunT(t2,t3)) ->
	 expect e1_typ t1 ; expect e2_typ t2 ;
	 t3
      | _ -> mismatch' "binary operator" opt)
  | If (cond,e1,e2) ->
     let cond_typ = typeof_ ctx tctx cond in
     let e1_typ = typeof_ ctx tctx e1 in
     let e2_typ = typeof_ ctx tctx e2 in
     expect cond_typ BoolT ; expect e1_typ e2_typ ;
     e1_typ
  | Pair (e1,e2) ->
     let e1_typ = typeof_ ctx tctx e1 in
     let e2_typ = typeof_ ctx tctx e2 in
     PairT (e1_typ, e2_typ)
  | Fst p ->
     let p_typ = typeof_ ctx tctx p in
     (match p_typ with
      | PairT (t,_) -> t
      | _ -> mismatch' "pair" p_typ)
  | Snd p ->
     let p_typ = typeof_ ctx tctx p in
     (match p_typ with
      | PairT (_,t) -> t
      | _ -> mismatch' "pair" p_typ)
  | EmptyList t ->
    let t = Util.normalize_type t in
    (match t with
     | ListT _ -> t
     | _ -> mismatch' "list" t)
  | Cons (hd,tl) ->
     let hd_typ = typeof_ ctx tctx hd in
     let tl_typ = typeof_ ctx tctx tl in
     (match tl_typ with
      | ListT t -> expect hd_typ t ; tl_typ
      | _ -> mismatch' "list" tl_typ)
     
  | Match (match_on, empty_case, hd_var, tl_var,cons_case) ->
     let match_on_typ = typeof_ ctx tctx match_on in
     (match match_on_typ with
      | ListT t ->
          let empty_typ = typeof_ ctx tctx empty_case in
          let new_ctx = SM.add hd_var t (SM.add tl_var (ListT t) ctx) in
	  let cons_typ = typeof_ new_ctx tctx cons_case in
	  expect empty_typ cons_typ ; empty_typ
      | _ -> mismatch' "list" match_on_typ)
     
  | Rec (name,arg,arg_typ,body_typ,body) ->
    let arg_typ = Util.normalize_type arg_typ in
    let body_typ = Util.normalize_type body_typ in   
    expectk TypeK (kindof_ tctx arg_typ);
    expectk TypeK (kindof_ tctx body_typ);   
    let rec_typ = FunT(arg_typ,body_typ) in
    let new_ctx = SM.add name rec_typ  (SM.add arg arg_typ ctx) in
    let body_typ' = typeof_ new_ctx tctx body in
    expect body_typ body_typ' ; rec_typ
    
  | Fun (arg,arg_typ,body) ->
    let arg_typ = Util.normalize_type arg_typ in
    expectk TypeK (kindof_ tctx arg_typ);
    let new_ctx = SM.add arg arg_typ ctx in
    FunT (arg_typ, typeof_ new_ctx tctx body)
  | App (e1,e2) ->
    let e1typ = typeof_ ctx tctx e1 in
     (match e1typ with
      | FunT (t_in,t_out) ->
 	 let t2 = typeof_ ctx tctx e2 in
	 expect t2 t_in ; t_out
      | _ -> mismatch' "function" e1typ)
  | TFun (v,k,e) ->
     let e_typ = typeof_ ctx (SM.add v k tctx) e in
     ForallT (v, k, e_typ)
  | TRec (f,v,k,t,body) ->
    let t = Util.normalize_type t in
    let f_typ = ForallT (v,k,t) in
    let body_typ = typeof_ (SM.add f f_typ ctx) (SM.add v k tctx) body in
    expect t body_typ; f_typ
    
  | TApp (e,t) ->
    let t = Util.normalize_type t in
    check_polytype t ;
    let e_typ = typeof_ ctx tctx e in
    (match e_typ with
     | ForallT (v, k, body_typ) ->
       expectk k (kindof_ tctx t);
       let subbed = Util.sub_in_typ body_typ v t in
        Util.normalize_type subbed
      | _ -> mismatch' "universal" e_typ)

  | TCase (op,alpha,
              eint,ebool,estr,evoid,
              efun,epair,elist) ->

    expectk (ArrowK (TypeK, TypeK)) (kindof_ tctx op) ;
    expectk TypeK (kindof_ tctx alpha) ;
    check_polytype alpha ;
    
    let apply_op t =
      Util.normalize_type (TAppT (op,t))
    in
    
    let tint = typeof_ ctx tctx eint in
    expect (apply_op IntT) tint ;
    
    let tbool = typeof_ ctx tctx ebool in
    expect (apply_op BoolT) tbool ;

    let tstr = typeof_ ctx tctx estr in
    expect (apply_op StrT) tstr ;

    let tvoid = typeof_ ctx tctx evoid in
    expect (apply_op VoidT) tvoid ;

    let tfun = typeof_ ctx tctx efun in
    expect
      (ForallT ("v1",TypeK,
                ForallT ("v2",TypeK,
                         apply_op (FunT (VarT "v1", VarT "v2")))))
      tfun ;
    
    let tpair = typeof_ ctx tctx epair in
    expect
      (ForallT ("v1",TypeK,
                ForallT ("v2",TypeK,
                         apply_op (PairT (VarT "v1", VarT "v2")))))
      tpair ;
    
    let tlist = typeof_ ctx tctx elist in
    expect
      (ForallT ("v",TypeK,apply_op (ListT (VarT "v"))))
      tlist ;

    apply_op alpha
    
    (*
  | TCase ((v,t),alpha,
              eint,ebool,estr,
              a,b,efun,
              c,d,epair,
              u,elist) ->

    let t = normalize_type t in
    
    let tbool = Util.sub_in_typ t v BoolT in
    let ebool_typ = typeof_ ctx tctx ebool in
    expect tbool ebool_typ ;

    let tint = Util.sub_in_typ t v IntT in
    let eint_typ = typeof_ ctx tctx eint in
    expect tint eint_typ ;

    let tfun = Util.sub_in_typ t v (FunT(VarT a,VarT b)) in
    let tctx' = SM.add b () (SM.add a () tctx) in
    let efun_typ = typeof_ ctx tctx' efun in
    expect tfun efun_typ ;

    let tpair = Util.sub_in_typ t v (PairT(VarT c,VarT d)) in
    let tctx' = SM.add d () (SM.add c () tctx) in
    let epair_typ = typeof_ ctx tctx' epair in
    expect tpair epair_typ ;

    let tlist = Util.sub_in_typ t v (ListT (VarT u)) in
    let tctx' = SM.add u () tctx in
    let elist_typ = typeof_ ctx tctx' elist in
    expect tlist elist_typ ;

    Util.sub_in_typ t v alpha*)
  | Closure _ | RecClosure _ -> raise (Type_error "can't typecheck a closure")
       
let typeof (e:exp) = typeof_ SM.empty SM.empty e
			     
