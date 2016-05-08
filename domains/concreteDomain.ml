
(*******
 * Abstract Interpretation project -- Semantics -- ENS L3 2016
 * By Théophile Bastian
 *******)

open Cfg
open Abstract_syntax_tree

(* module IntSet = Set.Make(struct type t=int let compare=compare end) *)

exception Not_implemented
exception UndefinedVariable of Cfg.var

module SingleState = struct
    type t = Z.t VarMap.t
    let compare = compare
end
module SiStSet = Set.Make(SingleState)

type t = SiStSet.t

let init l =
    SiStSet.singleton (List.fold_left (fun cur v -> VarMap.add v Z.zero cur)
        VarMap.empty l)

let bottom = SiStSet.empty

let is_bottom a =
    SiStSet.is_empty a

let rec evalExpr st = function
| CFG_int_unary (op,ex) -> (match op with
    | AST_UNARY_PLUS -> evalExpr st ex
    | AST_UNARY_MINUS -> Z.neg (evalExpr st ex))
| CFG_int_binary (op, ex1, ex2) ->
    let v1 = evalExpr st ex1 and v2 = evalExpr st ex2 in
    (match op with
    | AST_PLUS  -> Z.add v1 v2
    | AST_MINUS -> Z.sub v1 v2
    | AST_MULTIPLY -> Z.mul v1 v2
    | AST_DIVIDE -> Z.div v1 v2
    | AST_MODULO -> Z.rem v1 v2
    )
| CFG_int_var v ->
    (try VarMap.find v st
    with Not_found -> raise (UndefinedVariable v))
| CFG_int_const c ->
    c
| CFG_int_rand (low, high) ->
    raise Not_implemented

let assign dom var expr : SiStSet.t =
    if is_bottom dom then
        (* Raises UndefinedVariable if a variable is missing. *)
        SiStSet.singleton
            (VarMap.add var (evalExpr VarMap.empty expr) VarMap.empty)
    else
        SiStSet.fold (fun st cur -> (try
                SiStSet.add (VarMap.add var (evalExpr st expr) st) cur
            with UndefinedVariable _ -> cur))
            dom SiStSet.empty

let rec evalBoolExpr st = function
| CFG_bool_unary(op,ex) ->
    let v = evalBoolExpr st ex in
    (match op with
    | AST_NOT -> not v
    )
| CFG_bool_binary(op,ex1,ex2) -> 
    let v1 = evalBoolExpr st ex1 and v2 = evalBoolExpr st ex2 in
    (match op with
    | AST_AND -> v1 && v2
    | AST_OR -> v1 || v2
    )
| CFG_compare(cmp,ex1,ex2) ->
    let v1 = evalExpr st ex1 and v2 = evalExpr st ex2 in
    (match cmp with
    | AST_EQUAL -> Z.equal v1 v2
    | AST_NOT_EQUAL -> not (Z.equal v1 v2)
    | AST_LESS -> Z.lt v1 v2
    | AST_LESS_EQUAL -> Z.leq v1 v2
    | AST_GREATER -> Z.gt v1 v2
    | AST_GREATER_EQUAL -> Z.geq v1 v2
    )
| CFG_bool_const b -> b
| CFG_bool_rand -> raise Not_implemented

let guard dom expr =
    SiStSet.fold (fun env cur -> match evalBoolExpr env expr with
        | true -> SiStSet.add env cur
        | false -> cur)
        dom SiStSet.empty

let join a b =
    SiStSet.union a b

let widen a b =
    join a b

let subset a b =
    SiStSet.subset a b

let print chan a =
    let ft = Format.formatter_of_out_channel chan in
    SiStSet.iter (fun map ->
        Format.fprintf ft "[@.@[";
        VarMap.iter (fun var x ->
            Format.fprintf ft "%d (%s) |-> %d@\n" var.var_id var.var_name
                (Z.to_int x))
            map;
        Format.fprintf ft "@]]@.") a
