%{

open Common
open Lexing
open ParsedSyntax

exception Error

let get_fun_typ args ret_typ : typ =
  List.fold_right
       (fun (_,t) t_acc -> FunT (t,t_acc))
       args
       ret_typ

let get_tfun_typ args ret_typ : typ =
  List.fold_right
       (fun (id,k) acc -> ForallT (id,k,acc))
       args
       ret_typ

let get_tyop_kind args ret_kind : kind =
  List.fold_right
       (fun (_,k) acc -> ArrowK (k,acc))
       args
       ret_kind

let rec unpack_fun ids_and_types e : exp =
  match ids_and_types with
  | [] -> e
  | (id,t)::ids' -> Fun (id,t,unpack_fun ids' e)

let rec unpack_tfun ids_and_types e =
  match ids_and_types with
  | [] -> e
  | (id,k)::ids' -> TFun (id,k,unpack_tfun ids' e)

let rec unpack_rec f args ret_typ body =
  match args with
  | (a1,a1_typ)::args' ->
    let f_of_a1_typ = get_fun_typ args' ret_typ in
    Rec (f,a1,a1_typ,f_of_a1_typ,
         unpack_fun args' body)
  | [] -> raise (Failure "expected function argument")

let rec unpack_trec f ids ret_typ body =
  match ids with
  | (id1,id1_kind)::ids' ->
    let f_of_id1_typ = get_tfun_typ ids' ret_typ in
    TRec (f,id1,id1_kind,
            f_of_id1_typ,
            unpack_tfun ids' body)
  | [] -> raise (Failure "expected function argument")

let rec unpack_tfunt ids_and_kinds t =
  match ids_and_kinds with
  | [] -> t
  | (id,k)::ids' -> TFunT (id,k,unpack_tfunt ids' t)

let rec unpack_trect f ids ret_kind body =
  match ids with
  | (id1,id1_kind)::ids' ->
    let f_of_id1_kind = get_tyop_kind ids' ret_kind in
    TRecT (f,id1,id1_kind,
            f_of_id1_kind,
            unpack_tfunt ids' body)
  | [] -> raise (Failure "expected function argument")

let unpack_let_rec f args ret_typ e1 e2 : exp =
  let f_typ = get_fun_typ args ret_typ in
  let f_to_body = Fun (f,f_typ,e2) in
  match args with
  | (a1,a1_typ)::args' ->
    let f_of_a1_typ = get_fun_typ args' ret_typ in
    App(f_to_body,
	Rec (f,a1,a1_typ,f_of_a1_typ,unpack_fun args' e1))
  | [] -> raise (Failure "expected function argument")

let unpack_let f args ret_typ e1 e2 : exp =
  let f_typ = get_fun_typ args ret_typ in
  let f_to_body = Fun (f,f_typ,e2) in
  App (f_to_body, unpack_fun args e1)

let to_typ (t_opt:typ option) : typ =
  match t_opt with
  | None -> NoneT
  | Some t -> t
;;

let to_kind (k_opt:kind option) : kind =
  match k_opt with
  | None -> NoneK
  | Some k -> k
  ;;
%}

%token <int> INT
%token <string> STR
%token TRUE
%token FALSE
%token LET
%token REC
%token TREC
%token IN
%token ASSIGN
%token DOUBLE_COLON
%right DOUBLE_COLON
%token NIL
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token COLON
%token MATCH
%token WITH
%token VERT_BAR
%token END
%token FORALL
%token DOT
%token IF
%token THEN
%token ELSE
%token PLUS
%token MINUS
%token DIV
%token MOD
%token CONCAT
%token ARROW
%token BIG_ARROW
%token STAR
%left PLUS MINUS CONCAT
%left DIV
%right STAR ARROW
%left MOD
%token LESS
%token LESSEQ
%token EQ
%nonassoc LESS LESSEQ EQ
%token AND
%token OR
%left OR
%left AND
%token FUN
%token TFUN
%token TFUNT
%token TRECT
%token TCASE
%token TCASET
%token OF
%token <string> ID
%token COMMA
%right COMMA
%token FST
%token SND
%token EOF
%token BOOL_TYP
%token LIST_TYP
%token INT_TYP
%token STR_TYP
%token VOID_TYP
%start <ParsedSyntax.exp> parse_exp
%start <ParsedSyntax.typ> parse_typ
%%


parse_exp:
        | e = exp; EOF          { e }
        ;

(* parse a type from a file for debugging purposes *)
parse_typ:
        | t = typ; EOF          { t }
        ;
        
kind:
        | LPAREN; k = kind; RPAREN           { k }  
        | STAR                               { TypeK }   
        | k1 = kind; ARROW; k2 = kind        { ArrowK (k1,k2) }

has_kind:
        | DOUBLE_COLON; k = kind             { k }
        ;

type_arg:
        | LPAREN; id = ID; k = has_kind; RPAREN    { (id,k) }
        | id = ID;                                   { (id,NoneK) }
        
typ:
        | FORALL; var = type_arg; DOT;
t = typ                       { let (id,k) = var in
                                ForallT (id, k, t) }
        | TFUNT; args = nonempty_list(type_arg); BIG_ARROW; body = typ;
                      { unpack_tfunt args body }
        | TRECT; f = ID; args = nonempty_list(type_arg); ret_kind = option(has_kind);
                   BIG_ARROW; body = typ;         { unpack_trect f args (to_kind ret_kind) body }
        | TCASET; alpha = typ; OF; option(VERT_BAR);
          matches = separated_list(VERT_BAR,separated_pair(typ,BIG_ARROW,typ)); END
                                   { match matches with
                                     | [(IntT,tint);
                                        (BoolT,tbool);
                                        (StrT,tstr);
                                        (FunT(VarT a, VarT b),tfun);
                                        (PairT(VarT c, VarT d),tpair);
                                        (ListT(VarT f), tlist)] -> TCaseT (alpha,
                                                                             tint,tbool,tstr,
                                                                             TFunT (a,TypeK,
                                                                                     TFunT (b,TypeK,
                                                                                              tfun)),
                                                                             TFunT (c,TypeK,
                                                                                     TFunT (d,TypeK,
                                                                                             tpair)),
                                                                             TFunT (f,TypeK,tlist))
                                     | _ -> raise Error }
        | t = typ1                      { t }
        ;
typ1:
        | t1 = typ1; ARROW; t2 = typ1   { FunT (t1,t2) }
        | t1 = typ1; STAR; t2 = typ1   { PairT (t1,t2) }
        | t = typ2                      { t }
        ;

typ2:
        | t1 = typ2; LBRACK; t2 = typ3; RBRACK     { TAppT(t1,t2) }
        | LIST_TYP; t = typ3           { ListT t }         
        | t = typ3        { t }
        
typ3:
        | LPAREN; t = typ; RPAREN       { t }
        | BOOL_TYP                      { BoolT }
        | INT_TYP                       { IntT }
        | STR_TYP                       { StrT }
        | VOID_TYP                          { VoidT }
        | var = ID                      { VarT var }
        ;

has_typ:
        | COLON; t = typ             { t }
        ;

arg:
        | LPAREN; id = ID; t = has_typ; RPAREN    { (id,t) }
        | id = ID;                                   { (id,NoneT) }
                                                                                      

tcase_annot:
   | LBRACK; t = typ; RBRACK           { t }

exp:
        | LET; is_rec = boption(REC); f = ID; args = list(arg);
          ret_typ = option(has_typ); ASSIGN; e1 = exp; IN; e2 = exp
                                                                    { if is_rec
                                                                      then unpack_let_rec f args (to_typ ret_typ) e1 e2
                                                                      else unpack_let f args (to_typ ret_typ) e1 e2 }
        | FUN; args = nonempty_list(arg); BIG_ARROW; body = exp;
                                    { unpack_fun args body }
        | REC; f = ID; args = nonempty_list(arg);
          ret_typ = option(has_typ); BIG_ARROW; body = exp; 
                                    { unpack_rec f args (to_typ ret_typ) body }
        | TFUN; args = nonempty_list(type_arg); BIG_ARROW; body = exp;
                      { unpack_tfun args body }
        | TREC; f = ID; args = nonempty_list(type_arg); ret_typ = option(has_typ);
                    BIG_ARROW; body = exp;         { unpack_trec f args (to_typ ret_typ) body }
        | IF; cond = exp;
                THEN; then_exp = exp;
                ELSE; else_exp = exp;
                                        { If (cond, then_exp, else_exp) }
        | MATCH; e1 = exp; WITH;
          VERT_BAR; NIL;
          BIG_ARROW; e2 = exp; VERT_BAR;
          hd = ID; DOUBLE_COLON; tl = ID;
          BIG_ARROW; e3 = exp; END              { Match (e1,e2,hd,tl,e3) }
        | NIL; t = option(has_typ);                          
                                        { EmptyList (to_typ t) }
        | TCASE; annot = option(tcase_annot); t = typ; OF; option(VERT_BAR);
          matches = separated_list(VERT_BAR,separated_pair(typ,BIG_ARROW,exp)); END
                                   { match matches with
                                     | [(IntT,eint);
                                        (BoolT,ebool);
                                        (StrT,estr);
                                        (FunT(VarT a, VarT b),efun);
                                        (PairT(VarT c, VarT d),epair);
                                        (ListT(VarT f), elist)] -> TCase (to_typ annot,t,
                                                                             eint,ebool,estr,
                                                                             TFun (a,TypeK,
                                                                                     TFun (b,TypeK,
                                                                                              efun)),
                                                                             TFun (c,TypeK,
                                                                                     TFun (d,TypeK,
                                                                                             epair)),
                                                                             TFun (f,TypeK,elist))
                                     | _ -> raise Error }

        | e = exp2                            { e }
    
            
exp2:
        | e1 = exp2; PLUS ; e2 = exp2     { Op (e1, Plus, e2) }
        | e1 = exp2; MOD; e2 = exp2       { Op (e1,Mod,e2) }            
        | e1 = exp2; CONCAT; e2 = exp2    { Op (e1, Concat, e2) }  
        | e1 = exp2; MINUS ; e2 = exp2    { Op (e1, Minus, e2) }
        | e1 = exp2; DIV ; e2 = exp2      { Op (e1, Div, e2) }
        | e1 = exp2; STAR ; e2 = exp2  { Op (e1, Times, e2) }        
        | e1 = exp2; LESS ; e2 = exp2     { Op (e1, Less, e2) }
        | e1 = exp2; LESSEQ ; e2 = exp2   { Op (e1, LessEq, e2) }
        | e1 = exp2; EQ; e2 = exp2        { Op (e1, Eq, e2) }                        
        | e1 = exp2; AND ; e2 = exp2      { Op (e1, And, e2) }
        | e1 = exp2; OR ; e2 = exp2       { Op (e1, Or, e2) }                        
        | e1 = exp2; COMMA; e2 = exp2     { Pair (e1,e2) }
        | e1 = exp2; DOUBLE_COLON; e2 = exp2;     { Cons (e1,e2) }
        | e = exp3                      { e }                                      
        ;
                                       

exp3:
        | f = exp3; args = exp4;         { App (f,args) }
        | e = exp3; LBRACK; t = option(typ); RBRACK
                                        { TApp (e,to_typ t) }
        | FST; e = exp4                  { Fst e }
        | SND; e = exp4                  { Snd e }
        | e = exp4                      { e }
        ;

exp4:

        | LPAREN; e = exp; RPAREN       { e }
        | var = ID                      { Var var }
        | TRUE                          { Constant (Bool true) }
        | FALSE                         { Constant (Bool false) }
        | n = INT                       { Constant (Int n) }
        | s = STR                       { Constant (Str s) }
        ;
