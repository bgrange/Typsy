open Common
open Syntax

exception Inference_error

let rec check_kind s k : bool =
  match k with
  | NoneK -> false
  | TypeK -> true
  | ArrowK (k1,k2) -> check_kind s k1 && check_kind s k2
   

let check_typ s t : bool =
  Util.foldt
    (fun _ _ bs -> List.for_all (fun x -> x) bs)
    check_kind
    (fun _ t ->
       match t with
       | NoneT -> false
       | _ -> true)
    (fun s _ -> s)
    s
    t

let check_exp e : bool =
  Util.fold
    (fun _ _ bs -> List.for_all (fun x -> x) bs)
    check_kind
    check_typ
    (fun _ _ -> true)
    (fun s _ -> s)
    ()
    e

let infer (e:exp) : exp =
  if check_exp e then e else raise Inference_error
  
