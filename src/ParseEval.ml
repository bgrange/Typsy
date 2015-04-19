open Core.Std
open Lexing
open Parser

open Common
open TypedSyntax

exception SyntaxError of string

let pos_to_string pos =
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error parse_fun lexbuf =
  try
    parse_fun Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg ->
    let pos_str = pos_to_string lexbuf.lex_curr_p in
    let msg' = sprintf "%s: %s\n" pos_str msg in
    raise (SyntaxError msg')
  | Parser.Error ->
    let pos_str = pos_to_string lexbuf.lex_curr_p in
    let msg = sprintf "%s: syntax error\n" pos_str in
    raise (SyntaxError msg)

let parse parse_fun filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ret = parse_with_error parse_fun lexbuf in
  In_channel.close inx;
  ret
;;

let parse_exp filename : ParsedSyntax.exp =
  parse Parser.parse_exp filename

let parse_typ filename : ParsedSyntax.typ =
  parse Parser.parse_typ filename

let eval_file filename =
  let parsed = parse_exp filename in
  let exp = Convert.convert parsed in
  let typed_exp = Infer.infer exp in
  let _ = Typecheck.typeof typed_exp in
  let evald = Eval.eval typed_exp in
  print_endline ("eval'd expression:\n" ^ (Pretty.string_of_exp evald)) ;
  evald
  
;;

let debug_eval_file filename =
  let parsed_exp = parse_exp filename in
  print_endline ("parsed expression:\n" ^ (Show.show<ParsedSyntax.exp> parsed_exp)) ;
  let exp = Convert.convert parsed_exp in
  let typed_exp = Infer.infer exp in
  print_endline ("typed expression:\n" ^ (Pretty.string_of_exp typed_exp)) ;
  let _ = Typecheck.typeof typed_exp in
  let value = Eval.eval typed_exp in
  print_endline ("eval'd expression:\n" ^ (Pretty.string_of_exp value)) ;
  value
;;
