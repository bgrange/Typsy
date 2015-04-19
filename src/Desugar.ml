open Common
open TypedSyntax

let type_let e =
  Util.transform
    (fun _ e ->
       match e with
       | TLet (v,t,body) -> Util.sub_in_typ b

let desugar e =
  type_let e
