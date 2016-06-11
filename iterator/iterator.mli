
(*******
 * Abstract Interpretation project -- Semantics -- ENS L3 2016
 * By Théophile Bastian
 *******)

(** Generic type of iterator. Constructed using {!Make}. *)
module type S = sig
    type domType
    (** A type representing the abstract domain. *)
    
    type alarmExpr = BoolExpr of Cfg.bool_expr | IntExpr of Cfg.int_expr
    type alarm = {
        alarm_pos : Abstract_syntax_tree.position ;
        alarm_expr: alarmExpr ;
        alarm_dom : domType ;
        alarm_msg : string
    }

    exception InvalidFlow of Cfg.node
    (** Thrown when a problem is encountered in the flow graph *)

    exception InternalError of string
    (** Thrown when an internal problem is encountered. *)
    
    exception NoMainFunc
    (** Thrown when the given cfg does not contain a main function *)

    exception NotImplemented
    (** Thrown when an unimplemented feature is required. *)

    val run : Cfg.cfg -> (domType*int) Cfg.NodeMap.t * alarm list
    (** Iterates on the program until a fixpoint is reached,
     * returning for each control point a pair of the invariant gathered
     * and the number of times this CP was visited. *)
    
    val printAlarm : out_channel -> alarm -> unit
    (** Prints an alert for the given failed assertion on the given channel. *)
end

module Make(X : Domain.DOMAIN) : S with type domType = X.t
(** Constructs an iterator for a given domain. *)

