(* Main program:  runs our tests *)

open Core.Std
open Lexing
open Lexer       
open Parser
       
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
(*
let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)*)

(*let filename = "/Users/ben/Documents/school/independent-work/testfile.jew" ;;*)
let parse () : ParsedSyntax.exp =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  Parser.prog Lexer.read lexbuf
;;

let parsed_exp = parse () in
    
let typed_exp = Infer.infer parsed_exp in
    
let _ = Typecheck.typeof typed_exp in
let value = Eval.eval typed_exp in
print_endline (Printing.string_of_exp value)
