(* Main program:  runs our tests *)

open Core.Std
open ParseEval   ;;

(*       
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)*)

(*let filename = "/Users/ben/Documents/school/independent-work/testfile.jew" ;;*)

let default_file = "test/fact.myml" ;;

let do_it file_opt debug () =
  let f =
    match file_opt with
    | None -> default_file
    | Some file -> file
  in
  let eval = if debug
    then ParseEval.debug_eval_file
    else ParseEval.eval_file
  in
  ignore (eval f)
;;    

if not !Sys.interactive then (
  Command.basic ~summary:"Parse and evaluate OCaml-like code"
    Command.Spec.(empty
                  +> anon (maybe ("filename" %: file))
                  +> flag "--debug" no_arg ~doc:"debug mode")
    do_it
  |> Command.run
)


