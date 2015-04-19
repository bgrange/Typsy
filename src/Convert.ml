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
  | NoneK -> TS.NoneK

let get_tyop_kind args ret_kind =
  List.fold_right
       (fun (_,k) acc -> ArrowK (k,acc))
       args
       ret_kind

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
  | ForallT (args,t') -> convert_forallt args t'
  | VarT x -> TS.VarT x
  | TFunT (args,t') -> convert_tfunt args t'
  | TRecT (f,args,k,t') -> convert_trect f args k t'
  | TAppT (t1,t2) -> TS.TAppT (convert_typ t1,
                               convert_typ t2)
  | TCaseT (alpha,cases) -> convert_tcaset alpha cases
  | NoneT -> TS.NoneT
and convert_forallt ids_and_kinds t =
  match ids_and_kinds with
  | [] -> convert_typ t
  | (id,k)::ids' -> TS.ForallT (id,
                                convert_kind k,
                                convert_forallt ids' t)
and convert_tfunt ids_and_kinds t =
  match ids_and_kinds with
  | [] -> convert_typ t
  | (id,k)::ids' -> TS.TFunT (id,
                              convert_kind k,
                              convert_tfunt ids' t)

and convert_trect f ids ret_kind body =
  match ids with
  | (id1,id1_kind)::ids' ->
    let f_of_id1_kind = get_tyop_kind ids' ret_kind in
    TS.TRecT (f,id1,convert_kind id1_kind,
              convert_kind f_of_id1_kind,
              convert_tfunt ids' body)
  | [] -> raise (Conversion_error "expected function argument")
and convert_tcaset alpha cases =
  match cases with
  | [(IntT,tint);
     (BoolT,tbool);
     (StrT,tstr);
     (VoidT,tvoid);
     (FunT(VarT a, VarT b),tfun);
     (PairT(VarT c, VarT d),tpair);
     (ListT(VarT f), tlist)] -> TS.TCaseT (convert_typ alpha,
                                           convert_typ tint,
                                           convert_typ tbool,
                                           convert_typ tstr,
                                           convert_typ tvoid,
                                           TS.TFunT (a,TS.TypeK,
                                                     TS.TFunT (b,TS.TypeK,
                                                               convert_typ tfun)),
                                           TS.TFunT (c,TS.TypeK,
                                                     TS.TFunT (d,TS.TypeK,
                                                               convert_typ tpair)),
                                           TS.TFunT (f,TS.TypeK,
                                                     convert_typ tlist))
  | _ -> raise (Conversion_error "malformed Typecase")

let get_fun_typ args ret_typ =
  List.fold_right
       (fun (_,t) t_acc -> FunT (t,t_acc))
       args
       ret_typ

let get_tfun_typ args ret_typ =
  ForallT (args,ret_typ)

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
  | Fun (args,e') -> convert_fun args e'
  | Rec (f,args,t,e') -> convert_rec f args t e'
  | TFun (args,e') -> convert_tfun args e'
  | TApp (e',t) -> TS.TApp (convert e', convert_typ t)
  | TRec (f,args,t,e) -> convert_trec f args t e

  | Let (v,args,t,e1,e2) -> convert_let v args t e1 e2
  | LetRec (v,args,t,e1,e2) -> convert_letrec v args t e1 e2
  | TLet (v,args,t,e') -> convert_tlet v args t e'
  | TLetRec (v,args,k,t,e') -> convert_tletrec v args k t e'
  | TCase (t,alpha,cases) -> convert_tcase t alpha cases
and convert_fun ids_and_types e =
  match ids_and_types with
  | [] -> convert e
  | (id,t)::ids' -> TS.Fun (id,
                         convert_typ t,
                         convert_fun ids' e)

and convert_tfun ids_and_types e =
  match ids_and_types with
  | [] -> convert e
  | (id,k)::ids' -> TS.TFun (id,
                          convert_kind k,
                          convert_tfun ids' e)

and convert_rec f args ret_typ body =
  match args with
  | (a1,a1_typ)::args' ->
    let f_of_a1_typ = get_fun_typ args' ret_typ in
    TS.Rec (f,a1,
            convert_typ a1_typ,
            convert_typ f_of_a1_typ,
            convert_fun args' body)
  | [] -> raise (Conversion_error "expected function argument")

and convert_trec f ids ret_typ body =
  match ids with
  | (id1,id1_kind)::ids' ->
    let f_of_id1_typ = get_tfun_typ ids' ret_typ in
    TS.TRec (f,id1,
             convert_kind id1_kind,
             convert_typ f_of_id1_typ,
             convert_tfun ids' body)
  | [] -> raise (Conversion_error "expected function argument")

and convert_letrec f args ret_typ e1 e2 =
  let f_typ = get_fun_typ args ret_typ in
  let f_to_body = Fun ([(f,f_typ)],e2) in
  convert (App (f_to_body, Rec (f,args,ret_typ,e1)))

and convert_let f args ret_typ e1 e2 =
  let f_typ = get_fun_typ args ret_typ in
  let f_to_body = Fun ([(f,f_typ)],e2) in
  convert (App (f_to_body, Fun (args,e1)))

and convert_tlet v args t e' =
  TS.TLet (v,convert_tfunt args t,
           convert e')

and convert_tletrec f args k t e' =
  TS.TLet (f,convert_trect f args k t,
           convert e')

and convert_tcase t alpha cases =
  match cases with
  | [(IntT,eint);
     (BoolT,ebool);
     (StrT,estr);
     (VoidT,evoid);
     (FunT(VarT a, VarT b),efun);
     (PairT(VarT c, VarT d),epair);
     (ListT(VarT f), elist)] -> TS.TCase (convert_typ t, convert_typ alpha,
                                          convert eint,convert ebool,
                                          convert estr,convert evoid,
                                          TS.TFun (a,TS.TypeK,
                                                   TS.TFun (b,TS.TypeK,
                                                            convert efun)),
                                          TS.TFun (c,TS.TypeK,
                                                   TS.TFun (d,TS.TypeK,
                                                            convert epair)),
                                          TS.TFun (f,TS.TypeK,convert elist))
  | _ -> raise (Conversion_error "malformed typecase")
