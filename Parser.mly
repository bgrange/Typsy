%{
open ParsedSyntax
module SS = Set.Make(String)

let free_vars (e:exp) : SS.t =
  let rec aux e bound =
    match e with
      Var x ->
        if SS.mem x bound then SS.empty else SS.singleton x
    | Constant _ -> SS.empty
    | Op (e1,op,e2) ->
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | If (e1,e2,e3) ->
        SS.union (SS.union (aux e1 bound)
                           (aux e2 bound))
                           (aux e3 bound)
    | Fun (x,_,e') ->
        aux e' (SS.add x bound)
    | Pair (e1,e2) ->
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | Fst p -> aux p bound
    | Snd p -> aux p bound
    | EmptyList _ -> SS.empty 
    | Cons (hd,tl) -> 
        SS.union (aux hd bound)
                 (aux tl bound)
    | Match (e1,e2,x_hd,x_tl,e3) ->
        SS.union (SS.union (aux e1 bound)
                           (aux e2 bound))
                 (aux e3 (SS.add x_hd
				 (SS.add x_tl bound)))
    | Rec (name,arg,_,_,body) ->
       aux body (SS.add name (SS.add arg bound))
    | App (e1,e2) ->  
        SS.union (aux e1 bound)
                 (aux e2 bound)
    | TypLam (_,e') -> aux e' bound
    | TypApp (e',_) -> aux e' bound			   
  in aux e SS.empty
;;
  
let rec unpack_fun ids_and_types e : exp =
  match ids_and_types with
  | [] -> e
  | (id,t)::ids' -> Fun (id,t,unpack_fun ids' e)

let rec unpack_tfun args e =
  match args with
  | [] -> e
  | arg::args' -> TypLam (arg, unpack_tfun args' e)


let unpack_let f args ret_typ e1 e2 : exp =
  let f_typ = List.fold_right
		    (fun (_,t) t_acc -> FunTyp (t,t_acc))
		    args
		    ret_typ
  in
  let f_to_body = Fun (f,f_typ,e2) in
  if SS.mem f (free_vars e1) then
    match args with
    | (a1,a1_typ)::args' ->
       App(f_to_body,
	   Rec (f,a1,a1_typ,ret_typ,unpack_fun args' e1))
    | [] -> raise (Failure "expected function argument")
  else
    App (f_to_body,
	 unpack_fun args e1)


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
%nonassoc LESS LESSEQ
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
        ;
/*
type_arg:
        | LPAREN; a = type_arg; RPAREN               { a }
        | id = ID                               { id }
        ;
*/
exp:
        | LET; f = ID; args = list(arg); ret_typ = option(colon_then_typ); ASSIGN; e1 = exp; IN; e2 = exp
                                        { unpack_let f args (to_typ ret_typ) e1 e2 }
        | FUN; args = nonempty_list(arg); ARROW; body = exp;
                                        { unpack_fun args body }
        | TFUN; args = nonempty_list(ID); ARROW; body = exp;
                                        { unpack_tfun args body }
        | IF; cond = exp;
                THEN; then_exp = exp;
                ELSE; else_exp = exp;
                                        { If (cond, then_exp, else_exp) }
        | MATCH; e1 = exp; WITH;
          VERT_BAR; NIL;
          ARROW; e2 = exp; VERT_BAR;
          hd = ID; CONS; tl = ID;
          ARROW; e3 = exp;              { Match (e1,e2,hd,tl,e3) }
        | NIL; t = option(colon_then_typ);                          
                                        { EmptyList (to_typ t) }
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
