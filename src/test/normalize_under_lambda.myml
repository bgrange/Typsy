let type Const_Int = \\ (X::*) => Int in
let type Const_Int2 = \\ (X::*) => (\\ (X::*) => X) [Int] in
\ (T::(* -> *) -> *) =>
  (\ (f:T [Const_Int] -> Int) (x:T [Const_Int2]) => f x)
