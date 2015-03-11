open OUnit2
open EvalSyntax
open SharedSyntax  
open ParseEval
open Filename

(* Useful Constants *)
let zero = Constant (Int 0) 
let one = Constant (Int 1) 
let two = Constant (Int 2) 
let three = Constant (Int 3) 
let four = Constant (Int 4)
let five = Constant (Int 5)
		    
let rec listify (l:exp list) : exp =
  match l with
      [] -> EmptyList
    | hd::tl -> Cons(hd,listify tl)		    


let test_dir = "test"
let eval_file f = ParseEval.eval_file (concat test_dir f)

let test_map _ = assert_equal (eval_file "map.myml" ())
			      (listify [two;three;four;five])

let suite =
  "suite">:::
    ["test_map">::test_map]

let () =
  run_test_tt_main suite
;;      
