let test_cl =
  Let ("y", Constant (Int 5),
    Let ("y", Constant (Int 6),
      Rec ("f", "x",
        Var "y")));;
