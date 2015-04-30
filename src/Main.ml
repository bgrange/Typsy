(* Main program:  runs our tests *)

open Core.Std
open Parse_eval   ;;

let default_file = "test/tfun.myml" ;;

let do_it file_opt debug () =
  let f =
    match file_opt with
    | None -> default_file
    | Some file -> file
  in
  let eval = if debug
    then debug_eval_file
    else eval_file
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


