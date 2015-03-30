%{
open Common
open Lexing
open ParsedSyntax

exception Error

let rec unpack_fun ids_and_types e : exp =
  match ids_and_types with
  | [] -> e
  | (id,t)::ids' -> Fun (id,t,unpack_fun ids' e)

let rec unpack_tfun args e =
  match args with
  | [] -> e
  | arg::args' -> TypLam (arg, unpack_tfun args' e)


let get_fun_typ args ret_typ : typ =
  List.fold_right
       (fun (_,t) t_acc -> FunTyp (t,t_acc))
       args
       ret_typ

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
  | None -> NoTyp
  | Some t -> t
;;


%}

%token <int> INT	       
%token TRUE
%token FALSE
%token LET
%token REC  
%token IN
%token ASSIGN
%token CONS
%right CONS
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
%token ARROW
%token BIG_ARROW
%token PRODUCT
%left PLUS MINUS
%left DIV
%right PRODUCT ARROW
%token LESS
%token LESSEQ
%nonassoc LESS LESSEQ
%token FUN
%token TFUN
%token TYPECASE
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
%start <ParsedSyntax.exp> prog
%%


prog:
        | e = exp; EOF          { e }
        ;

typ:
        | FORALL; var = ID; DOT;
          t = typ                       { Forall (var,t) }
        | t = typ1                      { t }
        ;
typ1:
        | t1 = typ1; ARROW; t2 = typ1   { FunTyp (t1,t2) }
        | t1 = typ1; PRODUCT; t2 = typ1   { PairTyp (t1,t2) }
        | t = typ2                      { t }
        ;

typ2:
        | LPAREN; t = typ; RPAREN       { t }
        | BOOL_TYP                      { BoolTyp }
        | INT_TYP                       { IntTyp }
        | LIST_TYP; t = typ2             { ListTyp t }
        | var = ID                      { VarTyp var }
        ;

colon_then_typ:
        | COLON; t = typ             { t }
        ;

arg:
        | LPAREN; id = ID; COLON; t = typ; RPAREN    { (id,t) }
                                         | id = ID;                                   { (id,NoTyp) }

tcase_annot:
        | LBRACK; id = ID; t = typ; RBRACK           { (id,t) }

exp:
        | LET; is_rec = boption(REC); f = ID; args = list(arg);
          ret_typ = option(colon_then_typ); ASSIGN; e1 = exp; IN; e2 = exp
                                                                    { if is_rec
                                                                      then unpack_let_rec f args (to_typ ret_typ) e1 e2
                                                                      else unpack_let f args (to_typ ret_typ) e1 e2 }
        | FUN; args = nonempty_list(arg); BIG_ARROW; body = exp;
                                        { unpack_fun args body }
        | TFUN; args = nonempty_list(ID); BIG_ARROW; body = exp;
                                        { unpack_tfun args body }
        | IF; cond = exp;
                THEN; then_exp = exp;
                ELSE; else_exp = exp;
                                        { If (cond, then_exp, else_exp) }
        | MATCH; e1 = exp; WITH;
          VERT_BAR; NIL;
          BIG_ARROW; e2 = exp; VERT_BAR;
          hd = ID; CONS; tl = ID;
          BIG_ARROW; e3 = exp; END              { Match (e1,e2,hd,tl,e3) }
        | NIL; t = option(colon_then_typ);                          
                                        { EmptyList (to_typ t) }
        | TYPECASE; annot = option(tcase_annot); t = typ; OF; option(VERT_BAR);
          matches = separated_list(VERT_BAR,separated_pair(typ,BIG_ARROW,exp)); END
                                   { match matches with
                                     | [(BoolTyp,ebool);
                                        (IntTyp,eint);
                                        (FunTyp(VarTyp a, VarTyp b),efun);
                                        (PairTyp(VarTyp c, VarTyp d),epair);
                                        (ListTyp(VarTyp f), elist)] -> Typecase (annot,t,
                                                                                ebool,
                                                                                eint,
                                                                                a,b,efun,
                                                                                c,d,epair,
                                                                                f,elist)
                                     | _ -> raise Error }

        | e = exp2                            { e }
    
            
exp2:
        | e1 = exp2; PLUS ; e2 = exp2     { Op (e1, Plus, e2) }
        | e1 = exp2; MINUS ; e2 = exp2    { Op (e1, Minus, e2) }
        | e1 = exp2; DIV ; e2 = exp2      { Op (e1, Div, e2) }
        | e1 = exp2; PRODUCT ; e2 = exp2  { Op (e1, Times, e2) }        
        | e1 = exp2; LESS ; e2 = exp2     { Op (e1, Less, e2) }
        | e1 = exp2; LESSEQ ; e2 = exp2   { Op (e1, LessEq, e2) }
        | e1 = exp2; COMMA; e2 = exp2     { Pair (e1,e2) }
        | e1 = exp2; CONS; e2 = exp2;     { Cons (e1,e2) }
        | e = exp3                      { e }                                      
        ;
                                       

exp3:
        | f = exp3; args = exp4;         { App (f,args) }
        | e = exp3; LBRACK; t = option(typ); RBRACK
                                        { TypApp (e,to_typ t) }
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
        ;
