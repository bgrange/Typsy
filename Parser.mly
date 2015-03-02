%{ open ParsedSyntax ;; 
%}
    
%token <int> INT	       
%token TRUE
%token FALSE
%token LET
%token LETREC
%token IN
%token ASSIGN
%token CONS
%right CONS
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token COLON
%token MATCH
%token WITH
%token VERT_BAR
%token FORALL
%token DOT
%token IF
%token THEN
%token ELSE
%token PLUS
%token MINUS
%token DIV
%token ARROW
%token PRODUCT
%left PLUS MINUS
%left DIV
%right PRODUCT ARROW
%token LESS
%token LESSEQ
%token FUN
%token TFUN
%token <string> ID
%token COMMA
%right COMMA
%token FST
%token SND
%token EOF
%token BOOL_TYP
%token LIST_TYP
%token INT_TYP
%start <ParsedSyntax.exp option> prog
%%
prog:
        | EOF         { None }
        | e = exp   { Some e }
        ;

typ:
        | LPAREN; t = typ; RPAREN       { t }
        | BOOL_TYP                      { BoolTyp }
        | INT_TYP                       { IntTyp }
        | t1 = typ; ARROW; t2 = typ     { FunTyp (t1,t2) }
        | t1 = typ; PRODUCT; t2 = typ   { PairTyp (t1,t2) }
        | LIST_TYP; t = typ             { ListTyp t }
        | FORALL; var = ID; DOT;
          t = typ                       { Forall (var, t) }
        | var = ID                      { VarTyp var }
        ;

colon_then_typ:
        | COLON; t = typ             { t }
        ;

type_arg:
        | LPAREN; a = type_arg; RPAREN               { a }
        | id = ID                               { id }
        ;
arg:
        | LPAREN; a = arg; RPAREN                { a }
        | id = ID; t = option(colon_then_typ)    { (id,t) }
        ;
exp:
        | LPAREN; e = exp; RPAREN       { e }
        | var = ID                      { Var var }
        | TRUE                          { Constant (Bool true) }
        | FALSE                         { Constant (Bool false) }
        | n = INT                       { Constant (Int n) }
        | e1 = exp; PLUS ; e2 = exp     { Op (e1, Plus, e2) }
        | e1 = exp; MINUS ; e2 = exp    { Op (e1, Minus, e2) }
        | e1 = exp; DIV ; e2 = exp      { Op (e1, Div, e2) }
        | e1 = exp; PRODUCT ; e2 = exp  { Op (e1, Times, e2) }        
        | e1 = exp; LESS ; e2 = exp     { Op (e1, Less, e2) }
        | e1 = exp; LESSEQ ; e2 = exp   { Op (e1, LessEq, e2) }
        | IF; cond = exp;
                THEN; then_exp = exp;
                ELSE; else_exp = exp    { If (cond, then_exp, else_exp) }
        | LET; ids = nonempty_list(arg); ret_typ = option(colon_then_typ); ASSIGN; e1 = exp; IN; e2 = exp
                                        { Let (ids,ret_typ,e1,e2) }

        | e1 = exp; COMMA; e2 = exp     { Pair (e1,e2) }
        | FST; e = exp                  { Fst e }
        | SND; e = exp                  { Snd e }
        | MATCH; e1 = exp; WITH;
          VERT_BAR; LBRACK; RBRACK;
          ARROW; e2 = exp; VERT_BAR;
          hd = ID; CONS; tl = ID;
          ARROW; e3 = exp               { Match (e1,e2,hd,tl,e3) }
        | FUN; ids = nonempty_list(arg); ARROW; body = exp;            
                                        { Fun (ids,body) }
        | TFUN; ids = nonempty_list(type_arg); ARROW; body = exp;
                                        { TypLam (ids,body) }
        | e = exp; LBRACK; t = option(typ); RBRACK
                                        { TypApp (e,t) }
        | e1 = exp; e2 = exp;           { App (e1,e2) }
        | es = separated_list(CONS,exp); CONS; LBRACK; RBRACK;
          t_opt = option(colon_then_typ)
                                        { List.fold_right (fun e l -> Cons(e,l)) es
                                                          (EmptyList t_opt)
                                        }                  
        ;
