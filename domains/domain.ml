(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)

open Abstract_syntax_tree
open Cfg
  
module type DOMAIN =
  sig

    (* type of abstract elements *)
    (* an element of type t abstracts a set of mappings from variables
       to integers
     *)
    type t
    
    (* Checks whether two domains are equal. *)
    val equal: t -> t -> bool

    (* initial environment, with all variables initialized to 0 *)
    val init: var list -> t

    (* empty set of environments *)
    val bottom: t

    (* assign an integer expression to a variable *)
    val assign: t -> var -> int_expr -> t

    (* filter environments to keep only those satisfying the
     * boolean expression *)
    val guard: t -> bool_expr -> t

    (* abstract join *)
    val join: t -> t -> t

    (* widening *)
    val widen: t -> t -> t

    (* whether an abstract element is included in another one *)
    val subset: t -> t -> bool

    (* whether the abstract element represents the empty set *)
    val is_bottom: t -> bool
        
    (* prints *)
    val print: out_channel -> t -> unit
        
  end

