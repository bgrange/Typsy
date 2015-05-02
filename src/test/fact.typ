let fact (n:Int) : Int =
  let rec aux (n:Int) (acc:Int) : Int =
    if n <= 1 then acc
    else aux (n - 1) (n * acc)
  in
  aux n 1
in
fact 20 
