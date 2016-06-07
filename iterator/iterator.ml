
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

    val run : Cfg.cfg -> (domType*int) NodeMap.t
end

module Make(X : Domain.DOMAIN) = struct
    type domType = X.t
    
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

    let domOfArc arc dom = match arc.arc_inst with
    | CFG_skip _ -> dom
    | CFG_assign(v,expr) -> X.assign dom v expr
    | CFG_guard(expr) -> X.guard dom expr
    | CFG_assert(expr) -> dom (*TODO*)
    | CFG_call(func) -> raise NotImplemented
    
    let extractNodeOrder ord node =
        (try NodeMap.find node ord
        with Not_found ->
            raise (InternalError "Node not found in ordering."))

    let doWiden nodeOrder cNode fromNode prevDom dom =
        let fromOrder = extractNodeOrder nodeOrder fromNode in
        let cOrder = extractNodeOrder nodeOrder cNode in
        if shouldWiden fromOrder cOrder then
            X.widen prevDom dom
        else
            dom

    let updateNode nodeOrd node env curVisits =
        let prevDom = extractDomain env node in 
        let nDom : X.t = List.fold_left (fun cur arc -> 
                X.join cur
                    (doWiden nodeOrd node arc.arc_src prevDom
                        (domOfArc arc (extractDomain env arc.arc_src))))
            X.bottom node.node_in in
        NodeMap.add node (nDom, (curVisits+1)) env

    let rec iterate nodeOrd env worklist = match NodeSet.cardinal worklist with
    | 0 -> env
    | _ ->
        let node = NodeSet.choose worklist in
        (*
        Format.eprintf "At %d: worklist = @?" node.node_id;
        NodeSet.iter (fun x -> Format.eprintf "%d " x.node_id) worklist;
        Format.eprintf "@."; *)
        let precAbstract,precVisits = (try NodeMap.find node env with
                Not_found -> raise (InternalError
                    "Node not found in environment.")) in
        let nEnv = updateNode nodeOrd node env precVisits in

        Format.eprintf "Changed domain at %d:@." node.node_id;
        X.print stderr (extractDomain nEnv node);
        Format.eprintf "@."; 

        let nWorklist = NodeSet.remove node
            (match X.equal precAbstract (fst (NodeMap.find node nEnv)) with
            | true -> worklist
            | false ->
                (*
                Format.eprintf "Domain changed: from @."; 
                X.print stderr precAbstract;
                Format.eprintf "to@.";
                X.print stderr (NodeMap.find node nEnv);
                Format.eprintf "===@."; *)
                NodeSet.union worklist (NodeSet.of_list
                    (List.map (fun x -> x.arc_dst) node.node_out))
            ) in
        iterate nodeOrd nEnv nWorklist
            
    let run cfg =
        let nodeOrder = orderNodes cfg in

        let rec processInit node env =
            if cfg.cfg_init_exit.node_id = node.node_id then
                env
            else begin
                if List.length node.node_out <> 1 then
                    (* Invalid init node *)
                    raise (InvalidFlow node);
                let outArc = List.hd node.node_out in
                processInit outArc.arc_dst
                    (updateNode nodeOrder outArc.arc_dst env 0)
            end
        in
        
        let startEnv = List.fold_left (fun cur node ->
                NodeMap.add node (*(X.init cfg.cfg_vars)*) (X.bottom,0) cur)
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

        iterate nodeOrder preiterEnv startWL
end
