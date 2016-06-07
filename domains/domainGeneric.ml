open Cfg
open Abstract_syntax_tree

module Make(ValueX : Value_domain.VALUE_DOMAIN) = struct
    type t = ValueX.t VarMap.t
    
    let equal d1 d2 =
        VarMap.equal (=) d1 d2

    let init vars =
        List.fold_left (fun cur var ->
                VarMap.add var (ValueX.const (Z.zero)) cur)
            VarMap.empty vars

    let bottom = VarMap.empty

    let rec evalIntExpr dom = function
    | CFG_int_unary(unop, expr) ->
            ValueX.unary (evalIntExpr dom expr) unop
    | CFG_int_binary(binop,e1,e2) ->
            ValueX.binary (evalIntExpr dom e1) (evalIntExpr dom e2)
            binop
    | CFG_int_var(v) ->
            (try VarMap.find v dom
            with Not_found -> ValueX.bottom)
    | CFG_int_const(c) -> ValueX.const c
    | CFG_int_rand(low,high) -> ValueX.rand low high

    let assign dom var expr =
        VarMap.add var (evalIntExpr dom expr) dom
        
    let join d1 d2 =
        VarMap.merge (fun key v1 v2 -> (match v1,v2 with
            | None,None -> None
            | None,v | v,None -> v
            | Some sv1, Some sv2 -> Some (ValueX.join sv1 sv2))) d1 d2
            
    let meet =
        VarMap.merge (fun key v1 v2 -> (match v1,v2 with
            | None, None | None, Some _ | Some _, None -> None
            | Some sv1, Some sv2 -> Some (ValueX.meet sv1 sv2)))
        
    let rec bwdPropagate exp value dom = match exp with
    | CFG_int_unary(op,ex) ->
        let bwdVal = ValueX.bwd_unary (evalIntExpr dom ex) op value in
        bwdPropagate ex bwdVal dom
	| CFG_int_binary(op,ex1,ex2) ->
        let bwdLeft,bwdRight = ValueX.bwd_binary
            (evalIntExpr dom ex1) (evalIntExpr dom ex2)
            op value in
        meet
            (bwdPropagate ex1 bwdLeft dom)
            (bwdPropagate ex2 bwdRight dom)
	| CFG_int_var(var) -> VarMap.add var value dom
	| CFG_int_const(cst) -> dom
	| CFG_int_rand(lo,hi) -> dom

    exception NotElimFail
    let guard dom expr =
        let rec guardBool = function
        | CFG_bool_unary(unop,exp) ->
            (match unop with
            | AST_NOT -> raise NotElimFail
            )
        | CFG_bool_binary(binop,e1,e2) ->
            let dom1 = guardBool e1
            and dom2 = guardBool e2 in
            
            (match binop with
            | AST_AND -> meet dom1 dom2
            | AST_OR -> join dom1 dom2
            )
        | CFG_compare(cmp,e1,e2) ->
            let iv1 = evalIntExpr dom e1
            and iv2 = evalIntExpr dom e2 in
            
            let left,right = ValueX.compare iv1 iv2 cmp in
            meet
                (bwdPropagate e1 left dom)
                (bwdPropagate e2 right dom)
        | CFG_bool_const(b) ->
            (match b with
            | false -> bottom
            | true -> dom
            )
        | CFG_bool_rand ->
            bottom
        in
        
        guardBool (Helpers.notElim expr)
        
    let widen =
        VarMap.merge (fun key v1 v2 -> (match v1,v2 with
            | None,None -> None
            | Some v, None | None, Some v -> Some v
            | Some v1, Some v2 -> Some (ValueX.widen v1 v2)))

    let subset d1 d2 =
        VarMap.fold (fun key v cur -> match cur with
            | false -> false
            | true ->
                    (try
                        ValueX.subset (VarMap.find key d1) v
                    with Not_found ->
                        true))
            d2 true

    let is_bottom = VarMap.is_empty

    let print chan dom =
        VarMap.iter (fun k v ->
            Cfg_printer.print_var chan k;
            Printf.fprintf chan ": ";
            ValueX.print chan v;
            Printf.fprintf chan "\n") dom
end

module DomainInterval = Make(ValueInterval)
module DomainConstant = Make(ValueConstant)

