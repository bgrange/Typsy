open Common
module T = Type
module TS = TypedSyntax  
open ParsedSyntax

exception Inference_error ;;



let rec convert_typ (t:typ) : T.typ =
  match t with
  | BoolTyp -> T.BoolTyp
  | IntTyp -> T.IntTyp
  | StrTyp -> T.StrTyp                
  | FunTyp (t1,t2) -> T.FunTyp (convert_typ t1,
				 convert_typ t2)
  | PairTyp (t1,t2) -> T.PairTyp (convert_typ t1,
				   convert_typ t2)
  | ListTyp t' -> T.ListTyp (convert_typ t')
  | Forall (v,t') -> T.Forall (v,(convert_typ t'))
  | VarTyp x -> T.VarTyp x
  | NoTyp -> raise Inference_error
       
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
  | TypLam (v,e') -> TS.TypLam (v,infer e')
  | TypApp (e',t) -> TS.TypApp (infer e', convert_typ t)
  | TypRec (v1,v2,t,e) -> TS.TypRec (v1,v2,convert_typ t,infer e)
  | Typecase (annot,t,
              eint,ebool,estr,
              a,b,efun,
              c,d,epair,
              e,elist) ->
    (match annot with
     | Some (v,u) ->
       TS.Typecase ((v,convert_typ u),convert_typ t,
                    infer eint, infer ebool, infer estr,
                    a,b,infer efun,
                    c,d,infer epair,
                    e,infer elist)
     | None -> raise Inference_error)


	   
