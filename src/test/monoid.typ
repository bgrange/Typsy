// This type operator tests for equality between types
let type TEq (X::*) =
 Typerec TEq [X] :: * -> * of
 | Int => \\ (Y::*) =>
          Typerec TEq2 [Y] :: * of
          | Int => Int
	  | Bool => Void
	  | Str => Void
	  | Void => Void
	  | S -> T => Void
	  | S * T => Void
	  | List S => Void
	  end
 | Bool => \\ (Y::*) =>
           Typerec TEq2 [Y] :: * of
           | Int => Void
	   | Bool => Bool
	   | Str => Void
	   | Void => Void
	   | S -> T => Void
	   | S * T => Void
	   | List S => Void
	   end
 | Str => \\ (Y::*) =>
           Typerec TEq2 [Y] :: * of
           | Int => Void
	   | Bool => Void
	   | Str => Str
	   | Void => Void
	   | S -> T => Void
	   | S * T => Void
	   | List S => Void
	   end
 | Void => \\ (Y::*) => Void
 | S -> T => \\ (Y::*) =>
           Typerec TEq2 [Y] :: * of
           | Int => Void
	   | Bool => Bool
	   | Str => Void
	   | Void => Void
	   | S2 -> T2 => (TEq [S] [S2]) -> (TEq [T] [T2])
	   | S2 * T2 => Void	   
	   | List S2 => Void
	   end
 | S * T => \\ (Y::*) =>
           Typerec TEq2 [Y] :: * of
           | Int => Void
	   | Bool => Void
	   | Str => Void
	   | Void => Void
	   | S2 -> T2 => Void
	   | S2 * T2 => (TEq [S] [S2]) * (TEq [T] [T2])
           | List S2 => Void
	   end
 | List S => \\ (Y::*) =>
           Typerec TEq2 [Y] :: * of
           | Int => Void
	   | Bool => Void
	   | Str => Void
	   | Void => Void
	   | S2 -> T2 => Void
	   | S2 * T2 =>Void
	   | List S2 => List (TEq [S] [S2])
	   end
 end
in

// Monoid. I.e. types that we can add.
// Functions : X -> X can be composed with themselves, which is a
// kind of addition. This is where we use type equality
let type M (X::*) =
  Typerec M [X] :: * of
  | Int => Int
  | Bool => Bool
  | Str => Str
  | Void => Void
  | S -> T => (\\ (U::*) => U -> U) [TEq [S] [T]]
  | S * T => M[S] * M[T]
  | List S => List S
  end
in

let rec append (A::*) (l1:List A) (l2:List A) : List A =
  match l1 with
  | nil => l2
  | hd::tl => hd::(append [A] tl l2)
  end
in

// Generic addition on most things (including functions : X -> X)
// It will not typecheck if you give it something like Int -> Bool
let rec plus (X::*) : M[X] -> M[X] -> M[X] =
 typecase [\\ (T::*) => M[T] -> M[T] -> M[T]] X of
 | Int => (\ (x1:Int) (x2:Int) => x1 + x2)
 | Bool => (\ (b1:Bool) (b2:Bool) => b1 || b2)
 | Str => (\ (s1:Str) (s2:Str) => s1 ++ s2)
 | Void => (\ (v1:Void) => {} )
 | S -> T => (\ (f1:M[S -> T]) (f2:M[S->T]) => \ (x:TEq [S] [T]) => f1 (f2 x))
 | S * T => (\ (p1:M[S] * M[T]) (p2:M[S] * M[T]) => (plus [S] (fst p1) (fst p2),
            					     plus [T] (snd p2) (snd p2)))
 | List S => append [S]
 end
in
(plus [Int -> Int] (\ (x:Int) => x + 1) (\ (x:Int) => x * 5) 3,
 plus [List Int] (1::2::3::4::5::(nil:List Int)) (6::7::8::9::(nil:List Int)))
