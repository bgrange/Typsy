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
  | ':'      { COLON }
  | '*'      { STAR }	     
  | "->"     { ARROW }
  | "=>"     { BIG_ARROW }
  | "match"  { MATCH }
  | "with"   { WITH }
  | '|'      { VERT_BAR }	     
  | "forall" { FORALL }
  | "int"    { INT_TYP }
  | "str"    { STR_TYP }         
  | "bool"   { BOOL_TYP }
  | "list"   { LIST_TYP }
  | "void"   { VOID_TYP }
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
  | "&&"     { AND }
  | "||"     { OR }
  | "<="     { LESSEQ }
  | "++"     { CONCAT }
  | "fun"    { FUN }
  | "tfun"   { TFUN }
  | "trec"   { TREC }
  | "TFun"   { TFUNT }
  | "TRec"   { TRECT }
  | "Typecase"   { TCASET }
  | "fst"    { FST }
  | "snd"    { SND }
  | id       { ID (Lexing.lexeme lexbuf) }
  | ','      { COMMA }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
           
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
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
