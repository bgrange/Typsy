%{

open Common
open Lexing
open ParsedSyntax

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
%token TFUNT
%token TCASE
%token TRECT
%token OF
%token <string> ID
%token <string> TYP_ID
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
%token TYPE
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

type_param:
        | LPAREN; id = TYP_ID; k = has_kind; RPAREN    { (id,k) }
        | id = TYP_ID;  { (id,NoneK) }

type_arg:
        | LBRACK; t_opt = option(typ); RBRACK { to_typ t_opt }
        
typ:
        | FORALL; vars = nonempty_list(type_param); DOT; t = typ
                                   { ForallT (vars, t) }
        | TFUNT; params = nonempty_list(type_param); BIG_ARROW; body = typ;
                      { TFunT (params,body) }
        | TRECT;  name= TYP_ID; alpha = type_arg; k = has_kind; OF; option(VERT_BAR);
          matches = separated_list(VERT_BAR,separated_pair(typ,BIG_ARROW,typ)); END { TRecT (name,alpha,k,matches) }
        | t = typ1                      { t }
        ;
typ1:
        | t1 = typ1; ARROW; t2 = typ1   { FunT (t1,t2) }
        | t1 = typ1; STAR; t2 = typ1   { PairT (t1,t2) }
        | t = typ2                      { t }
        ;

typ2:
        | t1 = typ2; t2 = type_arg     { TAppT(t1,t2) }
        | LIST_TYP; t = typ3           { ListT t }         
        | t = typ3        { t }
        
typ3:
        | LPAREN; t = typ; RPAREN       { t }
        | BOOL_TYP                      { BoolT }
        | INT_TYP                       { IntT }
        | STR_TYP                       { StrT }
        | VOID_TYP                          { VoidT }
        | var = TYP_ID                      { VarT var }
        ;

has_typ:
        | COLON; t = typ             { t }
        ;

exp_param:
        | LPAREN; id = ID; t = has_typ; RPAREN    { (id,t) }
        | id = ID;                                   { (id,NoneT) }
                                                                                   
param:
        | idt = exp_param;        { let (id,t) = idt in 
                                        (id,T t) }
        | idk = type_param;        { let (id,k) = idk in
                                       (id,K k) }

                                                                                      
exp:
        | LET; REC; f = ID; params = list(param);
                ret_typ = option(has_typ); ASSIGN; e1 = exp; IN; e2 = exp { LetRec (f,params,to_typ ret_typ,e1,e2) }
        | LET; f = ID; params = list(param); ret_typ = option(has_typ); ASSIGN; e1 = exp; IN; e2 = exp
                                                                          { Let (f,params,to_typ ret_typ,e1,e2) }
        | LET; TYPE; f = TYP_ID; params = list(type_param);
                ASSIGN; t = typ; IN; e = exp { TLet (f,params,t,e) }

        | FUN; params = nonempty_list(param); BIG_ARROW; body = exp;
                                    { Fun (params,body) }
        | REC; FUN; f = ID; params = nonempty_list(param);
          ret_typ = option(has_typ); BIG_ARROW; body = exp; 
                                    { Rec (f,params,to_typ ret_typ, body) }
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
        | TCASE; annot = type_arg; t = typ; OF; option(VERT_BAR);
          matches = separated_list(VERT_BAR,separated_pair(typ,BIG_ARROW,exp)); END
                                   { TCase (annot,t,matches)  }

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
        | e = exp3; t = type_arg
                                        { TApp (e,t) }
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
