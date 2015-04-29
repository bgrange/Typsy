let rec map (A::*) (B::*) (f: A -> B) (l : List A) : List B =
  match l with
  | nil => nil:(List B)
  | hd::tl => (f hd) :: (map [A] [B] f tl)
  end
in
23
