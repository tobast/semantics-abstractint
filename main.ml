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

type analysisDomains = DomIntervals | DomConcrete
module DomSet = Set.Make(struct type t=analysisDomains let compare=compare end)

let domains = ref DomSet.empty
let writeDot = ref false 
let argParse = [
    "-concrete", (Arg.Unit(fun () ->
            domains := DomSet.add DomConcrete !domains)),
        "Use the concrete domain to analyze code. MAY NOT TERMINATE." ;
    "-interval", (Arg.Unit(fun () ->
            domains := DomSet.add DomIntervals !domains)),
        "Use toe intervals domain to analyze code." ;
    "-dot", Arg.Set(writeDot),
        "Writes the control flow graph (in Dot format) to 'cfg.dot'." ]
let usageStr = Sys.argv.(0) ^ " [options] file1.c file2.c ... fileN.c"

(** Default action taken from the provided main.ml *)
let dump filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  Printf.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot "cfg.dot" cfg

let printDomain printer dom =
    Cfg.NodeMap.iter (fun nd v ->
        Format.printf "NODE %d [%s]: @."
            Cfg.(nd.node_id) (Cfg_printer.string_of_position
                Cfg.(nd.node_pos));
        printer stdout v;
        Format.printf "@.") dom

(** Processes the given file using the given options *)
let process filename =
    let prog = File_parser.parse_file filename in
    let cfg = Tree_to_cfg.prog prog in
    if !writeDot then
        Cfg_printer.output_dot "cfg.dot" cfg;
    DomSet.iter (fun dom -> (match dom with
            | DomIntervals ->
                let res = IntervalIterator.run cfg in
                printDomain DomainInterval.print res
            | DomConcrete ->
                let res = ConcreteIterator.run cfg in
                printDomain ConcreteDomain.print res
        )) !domains
                

(* parses arguments to get filename *)
let main () =
  (*
  match Array.to_list Sys.argv with
  | _::filename::_ -> dump filename
  | _ -> invalid_arg "no source file specified"
  *)
    Arg.parse argParse process usageStr
let _ = main ()
