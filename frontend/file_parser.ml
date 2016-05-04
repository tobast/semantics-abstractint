(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Opens and parses a file given as argument.
*)

open Abstract_syntax_tree
open Cfg_printer
open Lexing

(* parsing, with nice error messages *)

let parse_file (filename:string) : prog =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    Parser.file Lexer.token lex
  with
  | Parser.Error ->
      Printf.eprintf "Parse error (invalid syntax) near %s\n" 
        (string_of_position lex.lex_start_p);
      failwith "Parse error"
  | Failure "lexing: empty token" ->
      Printf.eprintf "Parse error (invalid token) near %s\n" 
        (string_of_position lex.lex_start_p);
      failwith "Parse error"
