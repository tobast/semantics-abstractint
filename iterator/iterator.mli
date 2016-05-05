
(*******
 * Abstract Interpretation project -- Semantics -- ENS L3 2016
 * By Théophile Bastian
 *******)

(** Generic type of iterator. Constructed using {!Make}. *)
module type S = sig
    type domType
    (** A type representing the abstract domain. *)

    exception InvalidFlow of Cfg.node
    (** Thrown when a problem is encountered in the flow graph *)

    exception InternalError of string
    (** Thrown when an internal problem is encountered. *)
    
    exception NoMainFunc
    (** Thrown when the given cfg does not contain a main function *)

    exception NotImplemented
    (** Thrown when an unimplemented feature is required. *)

    val run : Cfg.cfg -> domType Cfg.NodeMap.t
    (** Iterates on the program until a fixpoint is reached,
     * returning the invariants gathered. *)
end

module Make(X : Domain.DOMAIN) : S with type domType = X.t
(** Constructs an iterator for a given domain. *)

