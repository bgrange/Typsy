open Common
module TS = TypedSyntax  
open ParsedSyntax

exception Inference_error ;;

let rec convert_kind (k:kind) : TS.kind =
  match k with
  | TypeK -> TS.TypeK
  | ArrowK (k1,k2) -> TS.ArrowK (convert_kind k1,
                                 convert_kind k2)
  | NoneK -> raise Inference_error

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
  | NoneT -> raise Inference_error
       
(* For now, don't do any actual type inference, just convert
ParsedSyntax to TypedSyntax assuming all type annotations are
   present *)
let rec infer (e:exp) : TS.exp =
  match e with
  | Var v -> TS.Var v   
  | Constant c -> TS.Constant c
  | Op (e1,op,e2) -> TS.Op (infer e1,op,infer e2)
  | If (e1,e2,e3) -> TS.If (infer e1,infer e2,infer e3)
  | Pair (e1,e2) -> TS.Pair (infer e1,infer e2)
  | Fst e' -> TS.Fst (infer e')
  | Snd e' -> TS.Snd (infer e')
  | EmptyList t -> TS.EmptyList (convert_typ t)
  | Cons (e1,e2) -> TS.Cons (infer e1, infer e2)
  | Match (e1,e2,v1,v2,e3) -> TS.Match (infer e1, infer e2,
					v1,v2, infer e3)
  | App (e1,e2) -> TS.App (infer e1,infer e2)
  | Fun (v,t,e') -> TS.Fun (v,convert_typ t, infer e')
  | Rec (v1,v2,t1,t2,e') -> TS.Rec(v1,v2,convert_typ t1,
				   convert_typ t2, infer e')
  | TFun (v,k,e') -> TS.TFun (v,convert_kind k,infer e')
  | TApp (e',t) -> TS.TApp (infer e', convert_typ t)
  | TRec (v1,v2,k,t,e) -> TS.TRec (v1,v2,convert_kind k,convert_typ t,infer e)
  | TCase (annot,t,
              eint,ebool,estr,
              efun,epair,elist) ->
       TS.TCase (convert_typ annot,convert_typ t,
                    infer eint, infer ebool, infer estr,
                    infer efun,infer epair,infer elist)


	   
