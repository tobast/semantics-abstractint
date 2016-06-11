open Abstract_syntax_tree
open Cfg

type bound = MInf | Int of Z.t | PInf

type t = Interv of bound * bound | Bottom

exception InvalidOperation
exception ZArithFail

let boundNeg = function
| MInf -> PInf
| Int z -> Int (Z.neg z)
| PInf -> MInf

let boundAbs = function
| MInf -> PInf
| PInf -> PInf
| Int z -> Int (Z.abs z)

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
let (<=@) a b = match a,b with
| MInf, _ | _,PInf -> true
| PInf,_ | _,MInf -> false
| Int z1, Int z2 -> Z.leq z1 z2

let bottomize = function
| Bottom -> Bottom
| Interv(a,b) as dom ->
        if b <@ a
            then Bottom
            else dom

let boundPred z = z -@ (Int Z.one)
let boundSucc z = z +@ (Int Z.one)

let boundMin a b =
    if a <@ b
        then a
        else b
let boundMax a b =
    if a <@ b
        then b
        else a

exception BadDomain
let domDivide d1 d2 = match d1,d2 with
| Bottom, _ | _, Bottom -> Bottom
| Interv(a,b), Interv(c,d) ->
    if (Int Z.zero) <@ c then
        Interv(boundMin (a /@ c) (a /@ d), boundMax (b /@ c) (b /@ d))
    else if d <@ (Int Z.zero) then
        Interv(boundMin (b /@ c) (b /@ d), boundMax (a /@ c) (a /@ d))
    else
        raise BadDomain

exception EmptyList
let listMin l =
    let rec get c = function
    | [] -> c
    | hd::tl ->
            if hd <@ c then
                get hd tl
            else
                get c tl
    in
    (match l with
    | [] -> raise EmptyList
    | x::[] -> x
    | l -> get (List.hd l) (List.tl l))
let listMax l =
    boundNeg (listMin (List.map boundNeg l))
    
let string_of_bound  = function 
| MInf -> "-∞"
| PInf -> "+∞"
| Int z -> (Z.to_string z)

let print chan dom =
    (match dom with
    | Bottom -> Printf.fprintf chan "⊥"
    | Interv(a,b) -> Printf.fprintf chan "[%s, %s]"
                (string_of_bound a) (string_of_bound b))


let top = Interv(MInf,PInf)

let bottom = Bottom

let const z = Interv(Int z, Int z)

let rand a b = Interv(Int a, Int b)

let join d1 d2 = bottomize (match (d1,d2) with
| Bottom, x | x, Bottom -> x
| Interv(a,b), Interv(c,d) ->
        Interv(boundMin a c, boundMax b d))
let meet d1 d2 = bottomize (match (d1,d2) with
| Bottom, x | x, Bottom -> Bottom
| Interv(a,b), Interv(c,d) ->
        Interv(boundMax a c, boundMin b d))

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
        Interv(listMin pairs, listMax pairs)
| AST_DIVIDE,(Interv(a,b) as num),(Interv(c,d) as den) ->
        let inf = meet den (Interv(MInf,Int(Z.minus_one))) in
        let sup = meet den (Interv(Int Z.one, PInf)) in
        join (domDivide num inf) (domDivide num sup)
        
| AST_MODULO,Interv(a,b),Interv(c,d) ->
        let bound = boundMax (boundAbs c) (boundAbs d) in
        join
            (if a <@ (Int Z.zero) then Interv(boundNeg bound, Int Z.zero)
                                  else Bottom)
            (if (Int Z.zero) <@ b then Interv(Int Z.zero, bound)
                                  else Bottom)

let rec compare d1 d2 op = match d1,d2 with
| Bottom,_ | _,Bottom -> Bottom,Bottom
| Interv(a,b), Interv(c,d) ->
    let swap (x,y) = (y,x) in
    (match op with
    | AST_EQUAL ->
            let m = meet d1 d2 in
            m,m
    | AST_NOT_EQUAL ->
            let isIn x a b = (a <=@ x && x <=@ b) in
            let diff a b c d =
                bottomize (Interv (
                    (if isIn a c d then d else a),
                    (if isIn b c d then c else b) ) )
            in
            (diff a b c d), (diff c d a b)
    | AST_LESS ->
            (bottomize (Interv (a, boundMin b (boundPred d)))),
            (bottomize (Interv (boundMax c (boundSucc a), d)))
    | AST_LESS_EQUAL ->
            (bottomize (Interv (a, boundMin b d))),
            (bottomize (Interv (boundMax c a, d)))
    | AST_GREATER ->
            swap (compare d2 d1 AST_LESS)
    | AST_GREATER_EQUAL ->
            swap (compare d2 d1 AST_LESS_EQUAL)
    )

let bwd_unary d1 op r = match d1,r with
| Bottom,_ | _,Bottom -> Bottom
| Interv(_,_), Interv(lo,hi) -> (match op with
    | AST_UNARY_PLUS -> meet d1 r
    | AST_UNARY_MINUS -> meet d1 (Interv(boundNeg hi, boundNeg lo))
)

let bwd_binary d1 d2 op r = match d1,d2,r with
| Bottom,_,_ | _,Bottom,_ | _,_,Bottom -> Bottom,Bottom
| Interv(a,b), Interv(c,d), Interv(lo,hi) -> (match op with
    | AST_PLUS ->
        meet d1 (binary r d2 AST_MINUS),
        meet d2 (binary r d1 AST_MINUS)
	| AST_MINUS ->
        meet d1 (binary r d2 AST_PLUS),
        meet d2 (binary d1 r AST_MINUS)
	| AST_MULTIPLY ->
        meet d1 (binary r d2 AST_DIVIDE),
        meet d2 (binary r d1 AST_DIVIDE)
	| AST_DIVIDE ->
        meet d1 (binary r d2 AST_MULTIPLY),
        meet d2 (binary d1 r AST_DIVIDE)
	| AST_MODULO -> d1,d2
)
    
let widen d1 d2 = match (d1,d2) with
| x, Bottom | Bottom, x -> x
| Interv(a,b), Interv(c,d) ->
        Interv( (if a <=@ c then a else MInf) ,
            (if d <=@ b then b else PInf) )
    
let narrow d1 d2 = match d1,d2 with
| Bottom,x | x,Bottom -> x
| Interv(l1,u1), Interv(l2,u2) ->
    Interv((if l1 = MInf then l2 else l1),
        (if u1 = PInf then u2 else u1))

let subset d1 d2 = match (d1,d2) with
| Bottom, _ -> true
| _, Bottom -> false
| Interv(a,b), Interv(c,d) ->
        (c <=@ a) && (b <=@ d)

let is_bottom = function
| Bottom -> true
| _ -> false

