let t = ParseEval.parse_typ "test/tcaset.myml";;

let t' = Convert.convert_typ t ;;

print_endline (Pretty.string_of_typ (Util.normalize_type t')) ;;
