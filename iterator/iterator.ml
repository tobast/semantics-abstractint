
(*******
 * Abstract Interpretation project -- Semantics -- ENS L3 2016
 * By Théophile Bastian
 *******)

open Cfg

module type S = sig
    type domType
    
    type assertFail = {
        assert_pos : Abstract_syntax_tree.position ;
        assert_expr : Cfg.bool_expr ;
        assert_dom : domType
    }

    exception InvalidFlow of Cfg.node
    exception InternalError of string
    exception NoMainFunc
    exception NotImplemented

    val run : Cfg.cfg -> (domType*int) NodeMap.t * assertFail list
    val printAssertFailed : out_channel -> assertFail -> unit
end

module Make(X : Domain.DOMAIN) = struct
    type domType = X.t
    type assertFail = {
        assert_pos : Abstract_syntax_tree.position ;
        assert_expr : Cfg.bool_expr ;
        assert_dom : domType
    }
    
    type nodeOrdering = {
        nodePos : int ;
        nodeCC : int ;
    }

    exception InvalidFlow of Cfg.node
    exception InternalError of string
    exception NoMainFunc
    exception NotImplemented
    
    let shouldWiden curPos jumpPos =
        curPos.nodeCC = jumpPos.nodeCC && curPos.nodePos > jumpPos.nodePos
    
    let orderNodes cfg =
        let rec walk node curList seen =
            match NodeSet.mem node seen with
            | true -> curList,seen
            | false ->
                let cSeen = NodeSet.add node seen in
                let nList,nSeen = List.fold_left (fun cur arc ->
                        walk arc.arc_dst (fst cur) (snd cur))
                    (curList,cSeen) node.node_out in
                (node::nList), nSeen
        in
        let rec fill cc map num = function
        | [] -> map
        | hd::tl -> fill cc
                (NodeMap.add hd {nodePos = num; nodeCC = cc} map)
                (num+1) tl
        in
        let fillCC node cc map =
            fill cc map 0 (fst (walk node [] NodeSet.empty))
        in
        
        let initMap = fillCC cfg.cfg_init_entry 0 NodeMap.empty in
        fst (List.fold_left (fun cur fct ->
                (fillCC fct.func_entry (snd cur) (fst cur)),(snd cur + 1))
            (initMap,1) cfg.cfg_funcs)
        

    let extractDomain env node =
        try fst (NodeMap.find node env) with
            Not_found -> raise (InternalError "Node not found in env.")
    let extractVisits env node =
        try snd (NodeMap.find node env) with
            Not_found -> raise (InternalError "Node not found in env.")

    let domOfFunc dom func =
        extractDomain dom func.func_exit

    let domOfArc arc dom fullDom = match arc.arc_inst with
    | CFG_skip _ -> dom
    | CFG_assign(v,expr) -> X.assign dom v expr
    | CFG_guard(expr) -> X.guard dom expr
    | CFG_assert(expr) -> dom (* This is not doing anything, only checks. *)
    | CFG_call(func) -> domOfFunc fullDom func
    
    let extractNodeOrder ord node =
        (try NodeMap.find node ord
        with Not_found ->
            raise (InternalError "Node not found in ordering."))

    let doWiden nodeOrder cNode fromNode prevDom dom =
        let fromOrder = extractNodeOrder nodeOrder fromNode in
        let cOrder = extractNodeOrder nodeOrder cNode in
        if shouldWiden fromOrder cOrder then
            if X.subset dom prevDom then
                X.narrow prevDom dom
            else
                X.widen prevDom dom
        else
            dom
            
    let getFctFilter filt cfg node =
        List.fold_left (fun cur fct ->
                if filt fct node then 
                    Some fct
                else
                    cur) None cfg.cfg_funcs
    let getFctEntry =
        getFctFilter (fun fct node -> fct.func_entry.node_id = node.node_id)
    let getFctExit =
        getFctFilter (fun fct node -> fct.func_exit.node_id = node.node_id)

    let updateNode cfg nodeOrd node env pEnv curVisits =
        let pDom = extractDomain env node in
        let nDom = List.fold_left (fun cur arc -> 
                let nVal = 
                    (doWiden nodeOrd node arc.arc_src
                        (domOfArc arc (extractDomain pEnv arc.arc_src) pEnv)
                        (domOfArc arc (extractDomain env arc.arc_src) env)) in
                X.join cur nVal)
            X.bottom
            (match getFctEntry cfg node with
            | None -> node.node_in
            | Some fct ->
                List.map (fun x -> { x with arc_inst = (CFG_skip "call") })
                    fct.func_calls)
        in
            
        NodeMap.add node (nDom, (curVisits+1)) env,
            NodeMap.add node (pDom, curVisits) pEnv
        
    let rec iterate cfg nodeOrd env pEnv once worklist =
        match NodeSet.cardinal worklist with
    | 0 -> env
    | _ ->
        let node = NodeSet.choose worklist in
        (*
        Format.eprintf "At %d: worklist = @?" node.node_id;
        NodeSet.iter (fun x -> Format.eprintf "%d " x.node_id) worklist;
        Format.eprintf "@."; *)
        let precVisits = extractVisits env node in
        let precAbstract = extractDomain pEnv node in
        let nEnv,npEnv = updateNode cfg nodeOrd node env pEnv precVisits in

		(*
        Format.eprintf "Changed domain at %d:@." node.node_id;
        X.print stderr (extractDomain nEnv node);
        Format.eprintf "@."; *)
        
        let arcUpdateNode arc = (match arc.arc_inst with
        | CFG_call(fct) -> fct.func_entry
        | _ -> arc.arc_dst) in

        let nWorklist = NodeSet.remove node
            (match X.equal precAbstract (fst (NodeMap.find node npEnv)),
                    NodeSet.mem node once with
            | true,false ->
                worklist
            | false,_ | true,true ->
                (*
                Format.eprintf "Domain changed: from @."; 
                X.print stderr precAbstract;
                Format.eprintf "to@.";
                X.print stderr (NodeMap.find node nEnv);
                Format.eprintf "===@."; *)
                NodeSet.union worklist (NodeSet.of_list
                    (match getFctExit cfg node with
                    | None -> List.map arcUpdateNode node.node_out
                    | Some fct ->
                            List.map (fun x -> x.arc_dst) fct.func_calls))
            ) in
        iterate cfg nodeOrd nEnv npEnv (NodeSet.remove node once) nWorklist
        
    let getAssertFails cfg dom =
        List.fold_left (fun cur arc -> match arc.arc_inst with
            | CFG_assert(expr) ->
                let cDom = extractDomain dom arc.arc_src in
                let gDom = X.guard cDom
                    (CFG_bool_unary(Abstract_syntax_tree.AST_NOT,expr)) in
                if X.is_bottom gDom then
					cur
                else
                    {
                        assert_pos = arc.arc_src.node_pos ;
                        assert_expr = expr ;
                        assert_dom = gDom
                    } :: cur
            | _ -> cur) [] cfg.cfg_arcs
            
    let run cfg =
        let nodeOrder = orderNodes cfg in
        let botEnv = List.fold_left (fun cur node ->
                NodeMap.add node (X.bottom,0) cur)
            NodeMap.empty cfg.cfg_nodes in

        let rec processInit node env =
            if cfg.cfg_init_exit.node_id = node.node_id then
                env
            else begin
                if List.length node.node_out <> 1 then
                    (* Invalid init node *)
                    raise (InvalidFlow node);
                let outArc = List.hd node.node_out in
                processInit outArc.arc_dst
                    (fst (updateNode cfg nodeOrder outArc.arc_dst
                        env botEnv 0))
            end
        in
        
(*        let startEnv = List.fold_left (fun cur node ->
                NodeMap.add node (X.init cfg.cfg_vars, 0) cur)
            NodeMap.empty cfg.cfg_nodes in *)
        let startEnv = NodeMap.add cfg.cfg_init_entry
            (X.init cfg.cfg_vars,0) botEnv in

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

        let outDom = iterate cfg nodeOrder preiterEnv botEnv
            (NodeSet.of_list cfg.cfg_nodes) startWL in
        
        outDom, getAssertFails cfg outDom
        
    let printAssertFailed ch asser =
        Printf.fprintf ch "ALERT: Assert ";
		Cfg_printer.print_bool_expr ch asser.assert_expr ;
		Printf.fprintf ch " at %s may have failed, \
			with domain\n"
            (Cfg_printer.string_of_position asser.assert_pos) ;
        X.print ch asser.assert_dom ;
        Printf.fprintf ch "\n"
end
