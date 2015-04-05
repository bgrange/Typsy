(* Main program:  runs our tests *)

open Core.Std
open ParseEval   ;;

let default_file = "test/typerec.myml" ;;

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


