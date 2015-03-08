{open Parser
open Lexing
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*	    
  
rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "let"    { LET }
  | "in"     { IN }
  | '='      { ASSIGN }
  | "::"     { CONS }
  | "nil"    { NIL }
  | '['      { LBRACK }
  | ']'      { RBRACK }	     
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | ':'      { COLON }
  | '*'      { PRODUCT }	     
  | "->"     { ARROW }
  | "match"  { MATCH }
  | "with"   { WITH }
  | '|'      { VERT_BAR }	     
  | "forall" { FORALL }
  | "int"    { INT_TYP }
  | "bool"   { BOOL_TYP }
  | "list"   { LIST_TYP }

  | '.'      { DOT }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '/'      { DIV }
  | '<'      { LESS }
  | "<="     { LESSEQ }
  | "fun"    { FUN }
  | "tfun"   { TFUN }
  | "fst"    { FST }
  | "snd"    { SND }
  | id       { ID (Lexing.lexeme lexbuf) }
  | ','      { COMMA }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
