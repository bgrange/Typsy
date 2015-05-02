let t1 = ParseEval.parse_typ "test/tcaset.myml";;
let t2 = ParseEval.parse_typ "test/tcaset2.myml" ;;

let t1' = Convert.convert_typ t1 ;;
let t2' = Convert.convert_typ t2 ;;

Util.typ_equiv t1' t2' ;;
