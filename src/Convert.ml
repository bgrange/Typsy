open Common
module Syn = Syntax  
open Parsed_syntax
open Util.GenVar

exception Conversion_error of string ;;

let rec convert_kind (k:kind) : Syn.kind =
  match k with
  | TypeK -> Syn.TypeK
  | ArrowK (k1,k2) -> Syn.ArrowK (convert_kind k1,
                                 convert_kind k2)
  | NoneK -> Syn.NoneK

let get_tyop_kind args ret_kind =
  List.fold_right
       (fun (_,k) acc -> ArrowK (k,acc))
       args
       ret_kind

let rec convert_typ (t:typ) : Syn.typ =
  match t with
  | BoolT -> Syn.BoolT
  | IntT -> Syn.IntT
  | StrT -> Syn.StrT
  | VoidT -> Syn.VoidT
  | FunT (t1,t2) -> Syn.FunT (convert_typ t1,
				 convert_typ t2)
  | PairT (t1,t2) -> Syn.PairT (convert_typ t1,
				   convert_typ t2)
  | ListT t' -> Syn.ListT (convert_typ t')
  | ForallT (args,t') -> convert_forallt args t'
  | VarT x -> Syn.VarT x
  | TFunT (args,t') -> convert_tfunt args t'
  | TAppT (t1,t2) -> Syn.TAppT (convert_typ t1,
                               convert_typ t2)
  | TRecT (name,alpha,k,cases) -> convert_trect name alpha k cases
  | NoneT -> Syn.NoneT
and convert_forallt ids_and_kinds t =
  match ids_and_kinds with
  | [] -> convert_typ t
  | (id,k)::ids' -> Syn.ForallT (id,
                                convert_kind k,
                                convert_forallt ids' t)
and convert_tfunt ids_and_kinds t =
  match ids_and_kinds with
  | [] -> convert_typ t
  | (id,k)::ids' -> Syn.TFunT (id,
                              convert_kind k,
                              convert_tfunt ids' t)
and convert_trect name alpha k cases =
  match cases with
  | [(IntT,tint);
     (BoolT,tbool);
     (StrT,tstr);
     (VoidT,tvoid);
     (FunT(VarT a, VarT b),tfun);
     (PairT(VarT c, VarT d),tpair);
     (ListT(VarT f), tlist)] ->
    let tfun = convert_typ tfun in
    let tfunAvoid = SS.add a (SS.add b (Util.free_tvars_in_typ tfun)) in
    let tfunV1 = gen_var tfunAvoid () in
    let tfunV2 = gen_var tfunAvoid () in
    let tfun = Util.replace_typ tfun (Syn.TAppT (Syn.VarT name, Syn.VarT a)) (Syn.VarT tfunV1) in
    let tfun = Util.replace_typ tfun (Syn.TAppT (Syn.VarT name, Syn.VarT b)) (Syn.VarT tfunV2) in

    let tpair = convert_typ tpair in    
    let tpairAvoid = SS.add a (SS.add b (Util.free_tvars_in_typ tpair)) in
    let tpairV1 = gen_var tpairAvoid () in
    let tpairV2 = gen_var tpairAvoid () in
    let tpair = Util.replace_typ tpair (Syn.TAppT (Syn.VarT name, Syn.VarT c)) (Syn.VarT tpairV1) in
    let tpair = Util.replace_typ tpair (Syn.TAppT (Syn.VarT name, Syn.VarT d)) (Syn.VarT tpairV2) in

    let tlist = convert_typ tlist in
    let tlistAvoid = SS.add f (Util.free_tvars_in_typ tlist) in
    let tlistV = gen_var tlistAvoid () in
    let tlist = Util.replace_typ tlist (Syn.TAppT (Syn.VarT name, Syn.VarT f)) (Syn.VarT tlistV) in

    let k = convert_kind k in
    Syn.TRecT (convert_typ alpha,
              convert_typ tint,
              convert_typ tbool,
              convert_typ tstr,
              convert_typ tvoid,
              Syn.TFunT (a,Syn.TypeK,
                        Syn.TFunT (b,Syn.TypeK,
                                  Syn.TFunT (tfunV1,k,
                                            Syn.TFunT (tfunV2,k,
                                                      tfun)))),
              Syn.TFunT (c,Syn.TypeK,
                        Syn.TFunT (d,Syn.TypeK,
                                  Syn.TFunT (tpairV1,k,
                                            Syn.TFunT (tpairV2,k,
                                                      tpair)))),
              Syn.TFunT (f,Syn.TypeK,Syn.TFunT (tlistV,k,tlist)))
  | _ -> raise (Conversion_error "malformed Typecase")


let get_fun_typ args ret_typ =
  List.fold_right
    (fun (id,tk) t_acc ->
       match tk with
       | T t -> FunT (t,t_acc)
       | K k -> ForallT ([(id,k)],t_acc))
    args
    ret_typ

let rec convert (e:exp) : Syn.exp =
  match e with
  | Var v -> Syn.Var v   
  | Constant c -> Syn.Constant c
  | Unop (op,e') -> Syn.Unop (op, convert e')
  | Binop (e1,op,e2) -> Syn.Binop (convert e1,op,convert e2)
  | If (e1,e2,e3) -> Syn.If (convert e1,convert e2,convert e3)
  | Pair (e1,e2) -> Syn.Pair (convert e1,convert e2)
  | Fst e' -> Syn.Fst (convert e')
  | Snd e' -> Syn.Snd (convert e')
  | EmptyList t -> Syn.EmptyList (convert_typ t)
  | Cons (e1,e2) -> Syn.Cons (convert e1, convert e2)
  | Match (e1,e2,v1,v2,e3) -> Syn.Match (convert e1, convert e2,
					v1,v2, convert e3)
  | App (e1,e2) -> Syn.App (convert e1,convert e2)
  | Fun (args,e') -> convert_fun args e'
  | Rec (f,args,t,e') -> convert_rec f args t e'
  | TApp (e',t) -> Syn.TApp (convert e', convert_typ t)

  | Let (v,args,t,e1,e2) -> convert_let v args t e1 e2
  | LetRec (v,args,t,e1,e2) -> convert_letrec v args t e1 e2
  | TLet (v,args,t,e') -> convert_tlet v args t e'
  | TCase (t,alpha,cases) -> convert_tcase t alpha cases
and convert_fun ids_and_types e =
  match ids_and_types with
  | [] -> convert e
  | (id,T t)::ids' -> Syn.Fun (id,
                         convert_typ t,
                              convert_fun ids' e)
  | (id,K k)::ids' -> Syn.TFun (id,
                               convert_kind k,
                               convert_fun ids' e)

and convert_rec f args ret_typ body =
  match args with
  | (a1,T a1_typ)::args' ->
    let f_of_a1_typ = get_fun_typ args' ret_typ in
    Syn.Rec (f,a1,
            convert_typ a1_typ,
            convert_typ f_of_a1_typ,
            convert_fun args' body)
  | (a1,K a1_kind)::args' ->
    let f_of_a1_typ = get_fun_typ args' ret_typ in
    Syn.TRec (f,a1,
             convert_kind a1_kind,
             convert_typ f_of_a1_typ,
             convert_fun args' body)
  
  | [] -> raise (Conversion_error "expected function argument")

and convert_letrec f args ret_typ e1 e2 =
  let f_typ = get_fun_typ args ret_typ in
  let f_to_body = Fun ([(f,T f_typ)],e2) in
  convert (App (f_to_body, Rec (f,args,ret_typ,e1)))

and convert_let f args ret_typ e1 e2 =
  let f_typ = get_fun_typ args ret_typ in
  let f_to_body = Fun ([(f,T f_typ)],e2) in
  convert (App (f_to_body, Fun (args,e1)))

and convert_tlet v args t e =
  let vdef = convert_tfunt args t in
  let e = convert e in
  Util.sub_typ_in_exp e v vdef

and convert_tcase t alpha cases =
  match cases with
  | [(IntT,eint);
     (BoolT,ebool);
     (StrT,estr);
     (VoidT,evoid);
     (FunT(VarT a, VarT b),efun);
     (PairT(VarT c, VarT d),epair);
     (ListT(VarT f), elist)] -> Syn.TCase (convert_typ t, convert_typ alpha,
                                          convert eint,convert ebool,
                                          convert estr,convert evoid,
                                          Syn.TFun (a,Syn.TypeK,
                                                   Syn.TFun (b,Syn.TypeK,
                                                            convert efun)),
                                          Syn.TFun (c,Syn.TypeK,
                                                   Syn.TFun (d,Syn.TypeK,
                                                            convert epair)),
                                          Syn.TFun (f,Syn.TypeK,convert elist))
  | _ -> raise (Conversion_error "malformed typecase")
