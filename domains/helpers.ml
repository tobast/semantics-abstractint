(**
 * Generic functions that do not belong to a particular place.
 **)

open Abstract_syntax_tree
open Cfg

(**
 * Eliminates [Abstract_syntax_tree.AST_NOT]s from the given expression,
 * using De Morgan's laws.
 **)
let notElim =
    let rec doElim negate = function
    | CFG_bool_unary(op,exp) ->
        (match op with
        | AST_NOT -> doElim (not negate) exp
        )
    | CFG_bool_binary(op,e1,e2) ->
        if negate then
            CFG_bool_binary(
                (match op with
                | AST_AND -> AST_OR
                | AST_OR -> AST_AND
                ),
                doElim negate e1, doElim negate e2)
        else
            CFG_bool_binary(op, doElim negate e1, doElim negate e2)
    | CFG_compare(cmp,e1,e2) ->
        if negate then
            CFG_compare(
                (match cmp with
                | AST_EQUAL -> AST_NOT_EQUAL
                | AST_NOT_EQUAL -> AST_EQUAL
                | AST_LESS -> AST_GREATER_EQUAL
                | AST_LESS_EQUAL -> AST_GREATER
                | AST_GREATER -> AST_LESS_EQUAL
                | AST_GREATER_EQUAL -> AST_LESS
                ), e1, e2)
        else
            CFG_compare(cmp, e1, e2)
    | CFG_bool_const(b) ->
        CFG_bool_const(if negate then not b else b)
    | CFG_bool_rand ->
        CFG_bool_rand
    in
    doElim false
