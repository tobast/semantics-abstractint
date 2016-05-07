
(*******
 * Abstract Interpretation project -- Semantics -- ENS L3 2016
 * By Théophile Bastian
 *******)

open Cfg

module type S = sig
    type domType

    exception InvalidFlow of Cfg.node
    exception InternalError of string
    exception NoMainFunc
    exception NotImplemented

    val run : Cfg.cfg -> domType NodeMap.t
end

module Make(X : Domain.DOMAIN) = struct
    type domType = X.t

    exception InvalidFlow of Cfg.node
    exception InternalError of string
    exception NoMainFunc
    exception NotImplemented

    let extractDomain env node =
        try NodeMap.find node env with
            Not_found -> raise (InternalError "Node not found in env.")

    let domOfArc arc dom = match arc.arc_inst with
    | CFG_skip _ -> dom
    | CFG_assign(v,expr) -> X.assign dom v expr
    | CFG_guard(expr) -> X.guard dom expr
    | CFG_assert(expr) -> dom (*TODO*)
    | CFG_call(func) -> raise NotImplemented

    let updateNode node env =
        let nDom = List.fold_left (fun cur arc -> 
                X.join cur (domOfArc arc (extractDomain env arc.arc_src)))
            X.bottom node.node_in in
        NodeMap.add node nDom env

    let rec iterate env worklist = match NodeSet.cardinal worklist with
    | 0 -> env
    | _ ->
        let node = NodeSet.choose worklist in
        let precAbstract = (try NodeMap.find node env with
                Not_found -> raise (InternalError
                    "Node not found in environment.")) in
        let nEnv = updateNode node env in

        let nWorklist = NodeSet.remove node
            (match precAbstract = (NodeMap.find node nEnv) with
            | true -> worklist
            | false -> NodeSet.union worklist
                (NodeSet.of_list (List.map (fun x -> x.arc_dst) node.node_out))
            ) in
        iterate nEnv nWorklist
            
    let run cfg =
        let rec processInit node env =
            if cfg.cfg_init_exit.node_id = node.node_id then
                env
            else begin
                if List.length node.node_out <> 1 then
                    (* Invalid init node *)
                    raise (InvalidFlow node);
                let outArc = List.hd node.node_out in
                processInit outArc.arc_dst (updateNode outArc.arc_dst env)
            end
        in

        let startEnv = List.fold_left (fun cur node ->
                NodeMap.add node (X.init cfg.cfg_vars) cur)
            NodeMap.empty cfg.cfg_nodes in

        (* Environment after global initializations. *)
        let postinitEnv = processInit (cfg.cfg_init_entry) startEnv in

        let mainFct = (try
                List.find (fun f -> f.func_name = "main") cfg.cfg_funcs
            with Not_found ->
                raise NoMainFunc) in
        let entryNode = mainFct.func_entry in
        
        (* Environment with global inits replicated at the entry point of main
         *)
        let preiterEnv = NodeMap.add entryNode
            (NodeMap.find cfg.cfg_init_exit postinitEnv) postinitEnv in

        let startWL = NodeSet.of_list
            (List.map (fun x -> x.arc_dst) entryNode.node_out) in

        iterate preiterEnv startWL
end
