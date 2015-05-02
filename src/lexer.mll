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
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let typ_id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
         
rule read =
         parse
       | white    { read lexbuf }
       | newline  { next_line lexbuf; read lexbuf }
       | "//"     { read_comment lexbuf }
       | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
       | '"'      { read_string (Buffer.create 100) lexbuf }
       | "true"   { TRUE }
       | "false"  { FALSE }
       | "let"    { LET }
       | "rec"    { REC }
       | "type"   { TYPE }          
       | "in"     { IN }
       | "typecase" { TCASE }    
       | "of"     { OF }
       | "end"    { END }     
       | '='      { ASSIGN }
       | "::"     { DOUBLE_COLON }
       | "nil"    { NIL }
       | '['      { LBRACK }
       | ']'      { RBRACK }	     
       | '('      { LPAREN }
       | ')'      { RPAREN }
       | '{'      { LBRACE }
       | '}'      { RBRACE }
       | ':'      { COLON }
       | ';'      { SEMI }
       | '*'      { STAR }	     
       | "->"     { ARROW }
       | "=>"     { BIG_ARROW }
       | "match"  { MATCH }
       | "with"   { WITH }
       | '|'      { VERT_BAR }	     
       | "Forall" { FORALL }
       | "Int"    { INT_TYP }
       | "Str"    { STR_TYP }         
       | "Bool"   { BOOL_TYP }
       | "List"   { LIST_TYP }
       | "Void"   { VOID_TYP }
       | '.'      { DOT }
       | "if"     { IF }
       | "then"   { THEN }
       | "else"   { ELSE }
       | '+'      { PLUS }
       | '-'      { MINUS }
       | '/'      { DIV }
       | '%'      { MOD }          
       | '<'      { LESS }
       | "=="     { EQ }
       | "=s="    { STREQ }
       | "=b="    { BOOLEQ } 
       | '>'      { GT }
       | ">="     { GTEQ }
       | '#'      { CHAR_AT }
       | "&&"     { AND }
       | "||"     { OR }
       | "<="     { LESSEQ }
       | "++"     { CONCAT }
       | '\\'    { FUN }
       | "\\\\"   { TFUNT }
       | "Typerec"    { TRECT }
       | "fst"    { FST }
       | "snd"    { SND }
       | "strlen" { STRLEN }
       | "value"  { VALUE }
       | "use"    { USE }
       | id       { ID (Lexing.lexeme lexbuf) }
       | typ_id   { TYP_ID (Lexing.lexeme lexbuf) }
       | ','      { COMMA }
       | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
       | eof      { EOF }

(* read_string was borrowed from
https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html*)
and read_string buf =
  parse
| '"'       { STR (Buffer.contents buf) }
| '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
| '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
| [^ '"' '\\']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf
  }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }

and read_comment =
  parse
| newline  { next_line lexbuf; read lexbuf }
| _        { read_comment lexbuf }
