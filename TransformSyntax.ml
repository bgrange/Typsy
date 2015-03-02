module PS = ParsedSyntax
module TS = TypedSyntax		
       
let rec unpack_fun ids_and_types e =
  match ids_and_types with
  | [] -> e
  | (id,typ_opt)::ids' -> TS.Fun (id,typ_opt,unpack_fun ids' e)
;;

let unpack_let ids_and_types e1 e2 =
  match ids_and_types with
  | [] -> raise (Failure "syntax error")
  | (id,typ_opt)::ids' -> TS.App (TS.Fun (id,typ_opt,e2),
				  unpack_fun ids' e1)
;;			       
