open TypedSyntax
open Common

let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Str s -> s


let string_of_op op = 
  match op with 
    | Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*"
    | Mod -> "%"
    | Div -> "/" 
    | Less -> "<" 
    | LessEq -> "<="
    | Eq -> "=="
    | And -> "&&"
    | Or -> "||"
    | Concat -> "++"

let rec string_of_kind k =
  match k with
  | TypeK -> "*"
  | ArrowK (k1,k2) -> Printf.sprintf
                        "(%s -> %s)"
                        (string_of_kind k1)
                        (string_of_kind k2)

let rec string_of_typ typ =
  match typ with
  | BoolT -> "bool"
  | IntT -> "int"
  | StrT -> "str"
  | FunT (a,b) -> Printf.sprintf "(%s -> %s)" (string_of_typ a) (string_of_typ b)
  | PairT (a,b) -> Printf.sprintf "(%s * %s)" (string_of_typ a) (string_of_typ b)
  | ListT a -> Printf.sprintf "list %s" (string_of_typ a)
  | VarT v -> v
  | ForallT (v,k,t) -> Printf.sprintf "forall %s::%s, %s"
                         v (string_of_kind k)
                         (string_of_typ t)
  | TAppT (t1,t2) -> Printf.sprintf
                      "%s %s"
                      (string_of_typ t1)
                      (string_of_typ t2)
                      
  | TFunT (v,k,t) -> Printf.sprintf
                     "(Tfun %s::%s => %s)"
                     v (string_of_kind k) (string_of_typ t)
  | TRecT (f,v,k1,k2,t) -> Printf.sprintf
                              "(Trec %s (%s::%s) :: %s => %s)"
                              f v (string_of_kind k1)
                              (string_of_kind k2)
                              (string_of_typ t)
  | TCaseT _ -> "<Typecase>"

(* Printing functions *)		      
		      
let max_prec = 10

let precedence e = 
  match e with 
    | Constant _ -> 0
    | Var _ -> 0
    | Op (_,Plus,_) -> 5
    | Op (_,Minus,_) -> 5
    | Op (_,Times,_) -> 3
    | Op (_,Mod,_) -> 2
    | Op (_,Div,_) -> 3
    | Op (_,Less,_) -> 7
    | Op (_,LessEq,_) -> 7
    | Op (_,Eq,_) -> 7
    | Op (_,And,_) -> 3
    | Op (_,Or,_) -> 5
    | Op (_,Concat,_) -> 7
    | If _ -> max_prec

    | Pair _ -> 0
    | Fst _ -> 1
    | Snd _ -> 1

    | EmptyList _ -> 0
    | Cons _ -> 8
    | Match _ -> max_prec

    | Rec _ -> max_prec
    | TRec _ -> max_prec
    | Fun _ -> max_prec		 
    | Closure _ -> max_prec
    | RecClosure _ -> max_prec		     
    | App _ ->  1

    | TFun _ -> max_prec
    | TApp _ -> 1
    | TCase _ -> max_prec

		    
let rec exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant c -> string_of_const c
      | Op (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(string_of_op op)^" "^(exp2string prec e2)
      | Var x -> x
      | If (e1, e2, e3) -> 
        "if " ^ (exp2string max_prec e1) ^ 
        " then " ^ (exp2string max_prec e2) ^ 
        " else " ^ (exp2string p e3)
      | Pair (e1, e2) -> 
	  "(" ^ (exp2string max_prec e1) ^ "," ^ (exp2string max_prec e2)  ^ ")"
      | Fst e1 ->  "fst " ^ (exp2string p e1)
      | Snd e1 ->  "snd " ^ (exp2string p e1)

      | EmptyList _ -> "[]"
      | Cons (e1,e2) -> (exp2string p e1) ^ "::" ^ (exp2string prec e2) 
      | Match (e1,e2,hd,tl,e3) -> 
	  "match " ^ (exp2string max_prec e1) ^ 
	    " with [] -> " ^ (exp2string max_prec e2) ^ 
            " | " ^ hd ^ "::" ^ tl ^ " -> " ^ (exp2string p e3)

      | Rec (f,x,tx,tbody,body) -> Printf.sprintf
                                     "rec %s (%s:%s) : %s => %s"
                                     f x (string_of_typ tx) (string_of_typ tbody)
                                     (exp2string max_prec body)
      | Fun (x,t,body) -> Printf.sprintf "fun (%s:%s) => %s" x
                            (string_of_typ t)
	                      (exp2string max_prec body)		     		  
      | App (e1,e2) -> Printf.sprintf "%s %s" (exp2string p e1) (exp2string p e2)
      | TFun (v,k,body) -> Printf.sprintf "tfun (%s::%s) => %s"
                               v (string_of_kind k) (exp2string p body)
      | TRec (f,x,k,t,body) -> Printf.sprintf "trec %s (%s::%s) : %s => %s"
                                   f x (string_of_kind k) (string_of_typ t)
                                   (exp2string max_prec body)
      | TApp (e',t) -> Printf.sprintf "%s [%s]" (exp2string p e') (string_of_typ t)
      | Closure _ | RecClosure _ -> "<closure>"
      | TCase (tyop,alpha,
                  eint,ebool,estr,evoid,
                  efun,
                  epair,
                  elist) -> "<typecase>"
(*
	Printf.sprintf "typecase [%s. %s] %s of\n\
                        | %s => %s\n\
                        | %s => %s\n\
                        | %s => %s\n\
                        | %s => %s\n\
                        | %s => %s"
          v (string_of_typ t) (string_of_typ alpha)
          (string_of_typ IntT) (exp2string p eint)
          (string_of_typ BoolT) (exp2string p ebool)
          (string_of_typ (FunT (VarT a,VarT b))) (exp2string p efun)
          (string_of_typ (PairT (VarT c,VarT d))) (exp2string p epair)
          (string_of_typ (ListT (VarT e))) (exp2string p elist) *)
  in 
  if p > prec then "(" ^ s ^ ")" else s

let string_of_exp e = exp2string max_prec e 
(*let string_of_env env = env2string env*)
