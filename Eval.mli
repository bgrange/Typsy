(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

(* evaluate input to a value *)
val eval : TypedSyntax.exp -> TypedSyntax.exp

(* evaluate input to a value while printing intermediate results *)
val debug_eval : TypedSyntax.exp -> TypedSyntax.exp
