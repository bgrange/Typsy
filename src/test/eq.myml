let type Eq (X::*) =
Typerec Eq [X] :: * of
| Int => Int
| Bool => Bool
| Str => Str
| Void => Void
| S -> T => Void
| S * T => (Eq [S]) * (Eq [T])
| List S => List (Eq [S])
end
in
let not (b:Bool) : Bool = if b then false else true in

let rec eq_list (A::*) (eq_a:A -> A -> Bool) (l1:List A) (l2:List A) : Bool =
    match l1 with
    | nil => match l2 with
             | nil => true
	     | hd::tl => false
	     end
    | hd1::tl1 => match l2 with
                | nil => false
		| hd2::tl2 => eq_a hd1 hd2 && eq_list [A] eq_a tl1 tl2
		end
    end
in
let rec eq (A::*) : Eq [A] -> Eq [A] -> Bool =
  typecase [\\ (D::*) => Eq[D] -> Eq[D] -> Bool] A of
  | Int => (\ (x:Int) (y:Int) => x == y)
  | Bool => (\ (b1:Bool) (b2:Bool) => b1 =b= b2)
  | Str => (\ (s1:Str) (s2:Str) => s1 =s= s2)
  | Void => (\ (v1:Void) (v2:Void) => false)
  | S -> T => (\ (f1:Void) (f2:Void) => false)
  | S * T => (\ (p1:Eq[S*T]) (p2:Eq[S*T]) => eq [S] (fst p1) (fst p2) && eq [T] (snd p1) (snd p2))
  | List S => eq_list [Eq[S]] (eq [S])
  end
in

let rec lookup (X::*) (Y::*) (default:Y) (l: List (Eq[X] * Y)) (x:Eq[X]) : Y =
  match l with
  | nil => default
  | hd::tl =>
      if eq [X] (fst hd) x
      then (snd hd)
      else lookup [X] [Y] default tl x
  end
in

let data : List (Str * Int) =
  ("bob",1)::("alli",12)::("joey",54)::("tom",32)::(nil:List (Str*Int))
in

lookup [Str] [Int] -1  data "alli"
