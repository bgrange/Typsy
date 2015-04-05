open Common

type typ =
    BoolTyp
  | IntTyp
  | StrTyp
  | FunTyp of typ * typ
  | PairTyp of typ * typ				
  | ListTyp of typ
  | Forall of variable * typ
  | VarTyp of variable
        deriving (Show)
