open SharedSyntax

module type ENV =
  sig
    type 'a t
    val empty : 'a t
    val filter : (variable -> bool) -> 'a t -> 'a t		   
    val lookup : 'a t -> variable -> 'a option
    val update : 'a t -> variable -> 'a -> 'a t
  end
;;

module ListEnv : ENV =
  struct
    type 'a t = (variable * 'a) list
		     
    let empty = [] 					   

    let filter f = List.filter (fun (v,_) -> f v) 
		  
    let rec lookup (ev:'a t) (v:variable) : 'a option =
      match ev with
      | [] -> None
      | (w,x)::ev' ->
	  if var_eq w v then Some x else lookup ev' v

    let update (ev:'a t) (v:variable) (e:'a) : 'a t =
      (v,e)::ev
  end
;;    

	

module Env = ListEnv
