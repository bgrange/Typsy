(rec \ f (x:Int) (y:Int) : Int => if x <= 0 then y else 1 + (f (x - 1) y)) 7 13
