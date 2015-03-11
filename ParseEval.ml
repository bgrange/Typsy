open Core.Std
open Lexing
open Lexer       
open Parser

open SharedSyntax
open Type
module PSyn = ParsedSyntax
module TSyn = TypedSyntax


let parse filename : ParsedSyntax.exp =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ret = Parser.prog Lexer.read lexbuf in
  In_channel.close inx;
  ret
;;

let eval_file filename () =
  let parsed = parse filename in
  let typed = Infer.infer parsed in
  let _ = Typecheck.typeof typed in
  let e = Util.erase_types typed in
  let evald = Eval.eval e in
  print_endline ("eval'd expression:\n" ^ (Pretty.string_of_exp evald)) ;
  evald
  
;;

let debug_eval_file filename () =
  let parsed_exp = parse filename in
  print_endline ("parsed expression:\n" ^ (Show.show<PSyn.exp> parsed_exp)) ;
  let typed_exp = Infer.infer parsed_exp in
  print_endline ("typed expression:\n" ^ (Show.show<TSyn.exp> typed_exp)) ;
  let _ = Typecheck.typeof typed_exp in
  let eval_exp = Util.erase_types typed_exp in
  let value = Eval.eval eval_exp in
  print_endline ("eval'd expression:\n" ^ (Pretty.string_of_exp value)) ;
  value
;;    
