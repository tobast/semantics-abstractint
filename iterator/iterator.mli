
(*******
 * Abstract Interpretation project -- Semantics -- ENS L3 2016
 * By Théophile Bastian
 *******)

(** Generic type of iterator. Constructed using {!Make}. *)
module type S = sig
    type domType
    (** A type representing the abstract domain. *)
    
    type assertFail = {
        assert_pos : Abstract_syntax_tree.position ;
        assert_expr : Cfg.bool_expr ;
        assert_dom : domType
    }

    exception InvalidFlow of Cfg.node
    (** Thrown when a problem is encountered in the flow graph *)

    exception InternalError of string
    (** Thrown when an internal problem is encountered. *)
    
    exception NoMainFunc
    (** Thrown when the given cfg does not contain a main function *)

    exception NotImplemented
    (** Thrown when an unimplemented feature is required. *)

    val run : Cfg.cfg -> (domType*int) Cfg.NodeMap.t * assertFail list
    (** Iterates on the program until a fixpoint is reached,
     * returning for each control point a pair of the invariant gathered
     * and the number of times this CP was visited. *)
    
    val printAssertFailed : out_channel -> assertFail -> unit
    (** Prints an alert for the given failed assertion on the given channel. *)
end

module Make(X : Domain.DOMAIN) : S with type domType = X.t
(** Constructs an iterator for a given domain. *)

