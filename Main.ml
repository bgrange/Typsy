(* Main program:  runs our tests *)


(* switch EvalEnv to EvalSubst to test substitution-based interpreter *)
let main = Testing.run_tests Eval.eval Testing.tests
