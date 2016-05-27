open Abstract_syntax_tree
open Cfg

type bound = MInf | Int of Z.t | PInf

type t = Interv of bound * bound | Bottom

exception InvalidOperation
exception ZArithFail

let boundNeg = function
| MInf -> PInf
| Int z -> Int z
| PInf -> MInf

let (+@) a b = match a,b with
| Int z1,Int z2 -> Int (Z.add z1 z2)
| MInf, Int _ | Int _, MInf -> MInf
| PInf, Int _ | Int _, PInf -> PInf
| PInf,PInf -> PInf
| MInf,MInf -> MInf
| MInf,PInf | PInf,MInf -> raise InvalidOperation

let (-@) a b = match a,b with
| Int z1,Int z2 -> Int (Z.sub z1 z2)
| MInf,PInf -> MInf
| PInf,MInf -> PInf
| MInf,Int _ | Int _,PInf -> MInf
| PInf,Int _ | Int _,MInf -> PInf
| MInf,MInf | PInf,PInf -> raise InvalidOperation


let ( *@ ) a b = match a,b with
| Int z1,Int z2 -> Int (Z.mul z1 z2)
| PInf,PInf | MInf,MInf -> PInf
| PInf,MInf | MInf,PInf -> MInf
| PInf,Int z | Int z,PInf -> (match Z.compare z Z.zero with
    | -1 -> MInf
    | 1 -> PInf
    | 0 -> Int (Z.zero)
    | _ -> raise ZArithFail)
| MInf,Int z | Int z,MInf -> (match Z.compare z Z.zero with
    | -1 -> PInf
    | 1 -> MInf
    | 0 -> Int (Z.zero)
    | _ -> raise ZArithFail)


let (/@) a b = match a,b with
| Int z1,Int z2 ->
    if z2 = Z.zero then
        raise InvalidOperation;
    Int (Z.div z1 z2)
| Int _,MInf | Int _,PInf| MInf,MInf|MInf,PInf|PInf,MInf|PInf,PInf ->
        Int (Z.zero)
| PInf, Int z -> (match (Z.compare z Z.zero) with
    | -1 -> MInf
    | 1 -> PInf
    | 0 -> raise InvalidOperation
    | _ -> raise ZArithFail)
| MInf, Int z -> (match (Z.compare z Z.zero) with
    | -1 -> PInf
    | 1 -> MInf
    | 0 -> raise InvalidOperation
    | _ -> raise ZArithFail)

let (<@) a b = match a,b with
| MInf, _ | _,PInf -> true
| PInf,_ | _,MInf -> false
| Int z1, Int z2 -> Z.lt z1 z2

let listMin l =
    let rec get c = function
    | [] -> c
    | hd::tl ->
            if hd (<@) c then
                get hd tl
            else
                get c tl

let top = Interv(MInf,PInf)

let bottom = Bottom

let const z = Interv(Int z, Int z)

let rand a b = Interv(Int a, Int b)

let unary iv op = match(iv, op) with
| Bottom,_ -> Bottom
| _,AST_UNARY_PLUS -> iv
| Interv(a,b),AST_UNARY_MINUS -> Interv(boundNeg b,boundNeg a)

let binary iv1 iv2 op = match op,iv1,iv2 with
| _,Bottom,_ | _,_,Bottom -> Bottom
| AST_PLUS,Interv(a,b),Interv(c,d) ->
        Interv(a +@ c, b +@ d)
| AST_MINUS,Interv(a,b),Interv(c,d) ->
        Interv(a -@ d, b -@ c)
| AST_MULTIPLY,Interv(a,b),Interv(c,d) ->
        let pairs = [ a *@ c ; a *@ d ; b *@ c ; b *@ d ] in

| AST_DIVIDE,Interv(a,b),Interv(c,d) ->
| AST_MODULO,Interv(a,b),Interv(c,d) ->

