let type S (X::*) =
  Typerec S [X] :: * of
  | Int => Int
  | Bool => Bool
  | Str => Str
  | Void => Void
  | B -> C => Void
  | B * C => S[B] * S[C]
  | List B => List (S[B])
  end
in
let rec int_to_string (n:Int) : Str =
  if n == 0 then "0"
  else if n == 1 then "1"
  else if n == 2 then "2"
  else if n == 3 then "3"
  else if n == 4 then "4"
  else if n == 5 then "5"
  else if n == 6 then "6"
  else if n == 7 then "7"
  else if n == 8 then "8"
  else if n == 9 then "9"
  else
    (int_to_string (n / 10)) ++ (int_to_string (n % 10))
in
let bool_to_string (b:Bool) : Str =
  if b then "true" else "false"
in
let rec fold (A::*) (B::*) (f:A -> B -> B) (base:B) (l:List A) : B =
      match l with
      | nil => base
      | hd::tl => (f hd (fold [A] [B] f base tl)) 
      end
in

let rec show (A::*) : S[A] -> Str =
 typecase [\\ (D::*) => (S[D] -> Str)] A of
 | Int => int_to_string
 | Bool => bool_to_string
 | Str => (\ (s:Str) => "\"" ++ s ++ "\"")
 | Void => (\ (v:Void) => "")
 | B -> C => (\ (v:Void) => "")
 | B * C => (\ (p: S[B * C]) => "(" ++ (show [B] (fst p)) ++ "," ++ (show [C] (snd p)) ++ ")")
 | List B => (\ (l:S[List B]) => fold [S[B]] [Str] (\ (x:S[B]) (s:Str) => (show [B] x) ++ s) "" l)
 end
in
show [Str] "12345"
