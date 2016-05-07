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

module ConcreteIterator = Iterator.Make(ConcreteDomain)

let domProcessor = ref (ConcreteIterator.run) 
let domPrinter = ref (ConcreteDomain.print)
let writeDot = ref false 
let argParse = [
    "-concrete", (Arg.Unit(fun () ->
            domProcessor := ConcreteIterator.run ;
            domPrinter := ConcreteDomain.print)),
        "Use the concrete domain to analyze code. MAY NOT TERMINATE." ;
    "-dot", Arg.Set(writeDot),
        "Writes the control flow graph (in Dot format) to 'cfg.dot'." ]
let usageStr = Sys.argv.(0) ^ " [options] file1.c file2.c ... fileN.c"

(** Default action taken from the provided main.ml *)
let dump filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  Printf.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot "cfg.dot" cfg

let printDomain dom =
    Cfg.NodeMap.iter (fun nd v ->
        Format.printf "NODE %d [%s]: @."
            Cfg.(nd.node_id) (Cfg_printer.string_of_position Cfg.(nd.node_pos));
        !domPrinter stdout v;
        Format.printf "@.") dom

(** Processes the given file using the given options *)
let process filename =
    let prog = File_parser.parse_file filename in
    let cfg = Tree_to_cfg.prog prog in
    if !writeDot then
        Cfg_printer.output_dot "cfg.dot" cfg;
    let domain = !domProcessor cfg in
    printDomain domain

(* parses arguments to get filename *)
let main () =
  (*
  match Array.to_list Sys.argv with
  | _::filename::_ -> dump filename
  | _ -> invalid_arg "no source file specified"
  *)
    Arg.parse argParse process usageStr
let _ = main ()
