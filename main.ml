(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Simple driver: parses the file given as argument and prints it back.

  You should modify this file to call your functions instead!
*)

open Iterator
open DomainGeneric

module ConcreteIterator = Iterator.Make(ConcreteDomain)
module IntervalIterator = Iterator.Make(DomainInterval)
module ConstantIterator = Iterator.Make(DomainConstant)

type analysisDomains = DomIntervals | DomConcrete | DomConstants
module DomSet = Set.Make(struct type t=analysisDomains let compare=compare end)

let domains = ref DomSet.empty
let writeDot = ref false 
let argParse = [
    "-concrete", (Arg.Unit(fun () ->
            domains := DomSet.add DomConcrete !domains)),
        "Use the concrete domain to analyze code. MAY NOT TERMINATE." ;
    "-constant", (Arg.Unit(fun () ->
            domains := DomSet.add DomConstants !domains)),
        "Use the constants domain to analyze code." ;
    "-interval", (Arg.Unit(fun () ->
            domains := DomSet.add DomIntervals !domains)),
        "Use the intervals domain to analyze code." ;
    "-dot", Arg.Set(writeDot),
        "Writes the control flow graph (in Dot format) to 'cfg.dot'." ]
let usageStr = Sys.argv.(0) ^ " [options] file1.c file2.c ... fileN.c"

(** Default action taken from the provided main.ml *)
let dump filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  Printf.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot "cfg.dot" cfg

let printDomain cfg printer dom =
    Cfg.NodeMap.iter (fun nd v ->
        let pointSuffix, overrideVar =
            (match Helpers.getFctEntry cfg nd, Helpers.getFctExit cfg nd with
            | Some fct, None -> " [ENTRY POINT: "^Cfg.(fct.func_name)^"]",[]
            | None, Some fct ->
                    " [EXIT POINT: "^Cfg.(fct.func_name)^"]",
                    (match Cfg.(fct.func_ret) with
                    | None -> []
                    | Some retVar -> [(retVar, "return value")])
            | None,None -> "",[]
            | Some _, Some _ -> " [WEIRD POINT: entry and exit. What \
                have you done?!]",[]
            ) in
        Format.printf "NODE %d [%s]: (took %d iterations)%s@."
            Cfg.(nd.node_id) (Cfg_printer.string_of_position
                Cfg.(nd.node_pos))
            (snd v) pointSuffix;
        printer overrideVar stdout (fst v);
        Format.printf "@.") dom
        
let printAsserts printer = function
| [] -> ()
| asserts ->
    List.iter (printer stdout) asserts
    

let printResult cfg printer assertPrinter (dom,asserts) =
    printAsserts assertPrinter asserts ;
    printDomain cfg printer dom ;
    if asserts <> [] then
        let assertLen = List.length asserts in
        Printf.printf "==============================\n\
            ALERT: %d alam%s raised! (See above)\n\
            ==============================\n"
            assertLen (if assertLen=1 then " was" else "s were")
    else
        Printf.printf "No alarm. Every assertion is correct.\n"

(** Processes the given file using the given options *)
let process filename =
    let prog = File_parser.parse_file filename in
    let cfg = Tree_to_cfg.prog prog in
    if !writeDot then
        Cfg_printer.output_dot "cfg.dot" cfg;
    DomSet.iter (fun dom -> (match dom with
            | DomIntervals ->
                let res = IntervalIterator.run cfg in
                printResult cfg DomainInterval.print
                    IntervalIterator.printAlarm res
            | DomConcrete ->
                let res = ConcreteIterator.run cfg in
                printResult cfg ConcreteDomain.print
                    ConcreteIterator.printAlarm res
            | DomConstants ->
                let res = ConstantIterator.run cfg in
                printResult cfg DomainConstant.print
                    ConstantIterator.printAlarm res
        )) !domains
                

(* parses arguments to get filename *)
let main () =
    Arg.parse argParse process usageStr
let _ = main ()
