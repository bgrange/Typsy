(* Testing *)

(* In this file, we build abstract syntax trees representing various
   functions for the purpose of testing our evaluators.

   You will have to construct some additional tests yourself.  See below.
 *)

open TypedSyntax
open Printing


(************)
(* INTEGERS *)
(************)

(* Useful Constants *)
let zero = Constant (Int 0) 
let one = Constant (Int 1) 
let two = Constant (Int 2) 
let three = Constant (Int 3) 
let four = Constant (Int 4)

(***********************)
(* RECURSIVE FUNCTIONS *)
(***********************)

(* let z = 2 in (let x = 3 in fun y -> x + y + z) (let x = 4 in x + z) *)
let clo =  
  Let ("z", two,
       App (Let ("x", three, 
		 Rec ("f", "y", IntTyp,IntTyp,
		      Op (Var "x", Plus, Op (Var "y", Plus, Var "z")))),
	    Let ("x", four, Op (Var "x", Plus, Var "z"))
       )
      )

(* rec fact n = if n < 1 then 1 else n * fact (n - 1) *)
let fact = 
  Rec ("fact", "n", IntTyp,IntTyp,
       If (Op (Var "n", Less, one),
           one,
           Op (Var "n", Times, 
               App (Var "fact", 
                        Op (Var "n", Minus, one)))))

(* fact 4 *)
let fact4 = App (fact, four)

(*********)
(* PAIRS *)
(*********)

(* the pair (1,2) *)
let p1 = Pair (one, two)

(* the function swap below is equivalent to:
       let swap p = let (x,y) = p in (y,x)
*)
let swap = 
  Rec ("swap", "p", PairTyp (IntTyp,IntTyp),PairTyp (IntTyp,IntTyp),
       Let ("x", Fst (Var "p"),
       Let ("y", Snd (Var "p"),
       Pair(Var "y", Var "x"))))

(* use swap to swap the elements of p1 *)
let swap_p1 = App (swap, p1)

(*********)
(* LISTS *)
(*********)

(* takes an OCaml list of expressions and generates a single expression
   representing the list
*)
let rec listify (l:exp list) (t:typ) : exp =
  match l with
      [] -> EmptyList t
    | hd::tl -> Cons(hd,listify tl t)

(* a list of 4 numbers *)
let list4 = listify [one;two;three;four] 

(* rec sumlist l = 
 *   match l with
 *     [] -> 0
 *   | hd::tl -> hd + sumlist tl *)
let sumlist = 
  Rec ("sumlist", "l", ListTyp IntTyp, IntTyp,
       Match (Var "l",
           zero,
           "hd", "tl", Op (Var "hd", Plus, 
			   App (Var "sumlist", Var "tl"))))

let sl4 = App (sumlist, list4)

(*******************************)
(* QUESTIONS FOR YOU TO ANSWER *)
(*******************************)

(* NOTE: NONE OF THE FUNCTIONS YOU WRITE BELOW SHOULD INCLUDE
 * Closure (env,f,x,e)
 *
 * Define recursive functions using the Rec(f,x,body) form
 *)

(* Replace the constant "one" below with your implementation of 
   the function map : ('a -> 'b) -> 'a list -> 'b list 
 *)
let map =
  Rec ("map", "f", FunTyp(IntTyp,IntTyp), FunTyp(ListTyp IntTyp,ListTyp IntTyp),
    Rec ("mapf", "l", ListTyp IntTyp, ListTyp IntTyp,
      Match (Var "l",
                    EmptyList IntTyp,
        "hd", "tl", Cons (App (Var "f", Var "hd"),
                          App (Var "mapf", Var "tl")))))
    
(* Replace the constant "one" below with your implementation of 
   a function "incr" that adds one to an integer *)
let incr : exp =
  Rec ("incr", "x", IntTyp,IntTyp,
       Op (Var "x", Plus, Constant (Int 1)))
;;

(* Use incr and map, defined above to implement the function incr_all
   below, which adds 1 to every element of a list. Examples:

   incr_all [] == []
   incr_all [1;2;3] == [2;3;4]
*)
let incr_all = App (map, incr) ;;
    

(* Replace the constant "one" below with your implementation of a function
 * that takes a list of pairs of integers and returns a list of 
 * integers where each element of the list is the sum of the elements of
 * the pairs.  Examples:

  sum_pairs [] == []
  sum_pairs [(1,2); (3,4)] == [3; 7]
 *)

let sum_pairs =
  App (map,
       Rec ("sum_pair", "p", PairTyp (IntTyp,IntTyp),IntTyp,
            Op (Fst (Var "p"), Plus, Snd (Var "p"))))
;;

(* Equivalent Ocaml: *)
let times5 =
  Let ("y", Constant (Int 5),
    Rec ("times5", "n", IntTyp,IntTyp,
      If (Op (Var "n", LessEq, Constant (Int 0)), Constant (Int 0), 
          Op (Var "y", Plus,
              Let ("y", Constant (Int 4), 
                   App (Var "times5", 
                        Op(Var "n", Minus, Constant (Int 1))))))))
;;

let app_times5 =
  App (times5, Constant (Int 6))
;;

(*let p = Pair(Constant (Int 1), Constant (Bool true)) ;;

let first = Fst p ;;
let second = Snd p ;;

let zip =
  Rec ("zip", "l1",
    Rec ("zip_l1", "l2",
      Match (Var "l1",
		EmptyList,
	"hd1", "tl1",
		Match (Var "l2",
			EmptyList,
		  "hd2", "tl2",
			Cons (Pair(Var "hd1", Var "hd2"),
			      App (App (Var "zip", Var "tl1"), Var "tl2"))))))
;;

let list5 = listify [two;three;four;one] ;;
let app_zip = App (App (zip, list4), list5) ;;

let to_pair ((n1,n2):int*int) : exp =
  Pair (Constant (Int n1), Constant (Int n2))
;;

let pairs = listify (List.map to_pair [(1,2);(2,3);(3,4)]) ;;
 *)
(*********)
(* TESTS *)
(*********)

(* Feel free to add many more tests of your own to the list *)
let tests = [zero; fact; fact4; list4; sl4; clo; incr_all;
		App (incr_all,list4); app_times5;
		(*p; first; second; zip; app_zip; App(sum_pairs, pairs)*)
		]

let run_test eval exp =
  Printf.printf "========\n";
  Printf.printf "%s\n" (string_of_exp exp);
  (try
    Printf.printf "%s\n" ("Has type: " ^ (string_of_typ (Typecheck.typeof exp)))
  with Typecheck.Type_error err ->
    Printf.printf "%s\n" err);
  Printf.printf "Evaluates to:\n";
  Printf.printf "%s\n" (string_of_exp (eval exp));
  Printf.printf "========\n"

let run_tests eval tests =
  List.iter (run_test eval) tests


