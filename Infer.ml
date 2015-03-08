module TS = TypedSyntax
open ParsedSyntax

exception Type_error of string ;;

let convert_op (op:operator) : TS.operator =
  match op with
  | Plus -> TS.Plus
  | Minus -> TS.Minus
  | Times -> TS.Times
  | Div -> TS.Div
  | Less -> TS.Less
  | LessEq -> TS.LessEq	      

let convert_const (c:constant) : TS.constant =
  match c with
  | Int n -> TS.Int n
  | Bool b -> TS.Bool b		    
		
let rec convert_typ (t:typ) : TS.typ =
  match t with
  | BoolTyp -> TS.BoolTyp
  | IntTyp -> TS.IntTyp
  | FunTyp (t1,t2) -> TS.FunTyp (convert_typ t1,
				 convert_typ t2)
  | PairTyp (t1,t2) -> TS.PairTyp (convert_typ t1,
				   convert_typ t2)
  | ListTyp t' -> TS.ListTyp (convert_typ t')
  | Forall (v,t') -> TS.Forall (v,(convert_typ t'))
  | VarTyp x -> TS.VarTyp x
  | NoTyp -> raise (Type_error "can't infer type")
       
(* For now, don't do any actual type inference, just convert
ParsedSyntax to TypedSyntax assuming all type annotations are
present *)
let rec infer (e:exp) : TS.exp =
  match e with
  | Var v -> TS.Var v   
  | Constant c -> TS.Constant (convert_const c)
  | Op (e1,op,e2) -> TS.Op (infer e1,convert_op op,infer e2)
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

	   
